#lang racket/base
; embed.rkt
; embed tags into images that support it
(require file/sha1
         gif-image
         ; identifier conflict with xml
         (prefix-in gif: gif-image/gif-basics)
         png-image
         racket/contract
         racket/file
         racket/format
         racket/list
         txexpr
         xml)
(provide add-embed-tags!
         dc:subject->list
         embed-support?
         get-embed-tags
         is-dc:subject?
         remove-embed!
         set-embed-tags!)

#|
xmp packet comments
#"<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>"
payload goes here
#"<?xpacket end=\"w\"?>"

PNG XMP keyword: #"XML:com.adobe.xmp"
JPEG XMP keyword: #"http://ns.adobe.com/xap/1.0/\0"
GIF XMP keyword: #"XMP Data" with auth #"XMP"
|#

; jpeg XMP string
(define jpeg-XMP-id #"http://ns.adobe.com/xap/1.0/\0")
(define SOI #xd8)
(define APP1 #xe1)
(define EOI #xd9)

; GIF XMP stuff
(define gif-XMP-id #"XMP Data")
(define gif-XMP-auth #"XMP")
(define gif-XMP-header (bytes-append gif-XMP-id gif-XMP-auth))

; takes a byte string and turns it into a decimal number
(define (bytes->number bstr)
  (string->number (bytes->hex-string bstr) 16))

; takes a decimal number and turns it into a byte string
(define (number->bytes num)
  (define str (~r num #:base 16 #:min-width 4 #:pad-string "0"))
  (hex-string->bytes str))

(define (jpeg-xmp? bstr)
  (bytes=? (subbytes bstr 4 (+ (bytes-length jpeg-XMP-id) 4)) jpeg-XMP-id))

(define (jpeg-has-marker? in marker-byte)
  (and (regexp-try-match (byte-regexp (bytes #xff marker-byte)) in)
       #t))

#|     JPEG stuff     |#

; returns a list of position pairs via regexp-match-positions*
(define/contract (jpeg-goto-marker in marker-byte)
  (bytes? byte? . -> . (or/c (listof pair?) empty?))
  (regexp-match-positions* (byte-regexp (bytes #xff marker-byte)) in))

(define/contract (jpeg? img)
  (any/c . -> . boolean?)
  (cond [(path-string? img)
         (define img-in (open-input-file img))
         (define jpg? (jpeg-has-marker? img-in SOI))
         (close-input-port img-in)
         jpg?]
        [(bytes? img)
         (define byts (subbytes img 0 2))
         (bytes=? byts (bytes #xff SOI))]
        [else #f]))

; returns a list of APP1 bytes
(define/contract (jpeg-get-app1 img)
  (jpeg? . -> . (listof bytes?))
  (define img-bytes (if (bytes? img)
                        img
                        (file->bytes img)))
  (define positions (jpeg-goto-marker img-bytes APP1))
  (for/list ([pair (in-list positions)])
    (define bstr (subbytes img-bytes (cdr pair) (+ (cdr pair) 2)))
    (define len (bytes->number bstr))
    (subbytes img-bytes (car pair) (+ (car pair) len 2))))

#|     GIF stuff     |#

(define (read-gif x)
  (cond [(bytes? x) x]
        [(path-string? x)
         (call-with-input-file x (lambda (in) (read-bytes (file-size x) in)))]
        [else (error "read-gif: invalid input:" x)]))

(define (get-appn-pos x)
  (define data (read-gif x))
  (let loop ([n 0]
             [lst '()])
    (cond [(gif:trailer? data n) lst]
          [(gif:img? data n)
           (loop (+ n (gif:img-size data n)) lst)]
          [(gif:gce? data n)
           (loop (+ n (gif:gce-size data n)) lst)]
          [(gif:appn? data n)
           (define size (gif:appn-size data n))
           (loop (+ n size) (cons (list n size) lst))]
          [(gif:comment? data n)
           (loop (+ n (gif:comment-size data n)) lst)]
          [(gif:plain-text? data n)
           (loop (+ n (gif:plain-text-size data n)) lst)]
          [(gif:header? data n)
           (loop (+ n (gif:header-size data)) lst)]
          [else (loop (+ n 1) lst)])))

(define (gif-get-appn x)
  (define data (read-gif x))
  (define appn-lst (get-appn-pos data))
  (for/list ([appn (in-list appn-lst)])
    (define pos (first appn))
    (define len (second appn))
    (subbytes data pos (+ pos len))))

; takes the appn byte string and determines if it's XMP
(define (gif-appn-xmp? bstr)
  (bytes=? (subbytes bstr 3 (+ (bytes-length gif-XMP-header) 3))
           gif-XMP-header))

; generate appn byte string, ready to be inserted into the image
(define (make-xmp-appn bstr)
  (bytes-append
   ; gif extension number + application extension label
   (bytes #x21 #xff)
   ; length of header + auth
   (bytes (bytes-length gif-XMP-header))
   gif-XMP-header
   bstr
   #"\1"
   (apply bytes
          (for/list ([magic (in-range #xff #x00 -1)]) magic))
   #"\0\0"))

#|     SVG stuff     |#

(define (svg? img)
  (define bstr (if (bytes? img)
                   img
                   (file->bytes img)))
  (bytes=? (subbytes bstr 0 13) #"<?xml version"))

(define (svg-has-tag? in bstr)
  (and (regexp-try-match (byte-regexp bstr) in) #t))

#|     Embedding stuff     |#

(define/contract (embed-support? img)
  (any/c . -> . boolean?)
  (or (gif? img) (jpeg? img) (png? img) (svg? img)))

(define/contract (add-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(gif? img) (add-embed-gif! img taglist)]
        [(jpeg? img) (add-embed-jpeg! img taglist)]
        [(png? img) (add-embed-png! img taglist)]
        [(svg? img) (add-embed-svg! img taglist)]))

; adds taglist to the existing tags
; if there are no existing tags, set them
(define (add-embed-png! png taglist)
  (define png-hash (png->hash png))
  ; grab the old XMP data as an XEXPR
  (define old-xmp (get-embed-png png-hash))
  (define old-tags (get-embed-tags png))
  (define reconciled (remove-duplicates (append old-tags taglist)))
  (define xexpr (if (empty? old-xmp)
                    ; if the image has no XMP data, generate some
                    (make-xmp-xexpr taglist)
                    (set-dc:subject (string->xexpr old-xmp) reconciled)))
  (define str (xexpr->xmp xexpr))
  (define itxt-bstr (make-itxt-chunk "XML:com.adobe.xmp" str))
  (define itxt-hash (make-itxt-hash itxt-bstr))
  (define new-hash (itxt-set png-hash itxt-hash "XML:com.adobe.xmp"))
  (define new-png (hash->png new-hash))
  (with-output-to-file png
    (λ ()
      (display new-png))
    #:mode 'binary
    #:exists 'truncate/replace))

(define (add-embed-jpeg! jpeg taglist)
  (define jpeg-bytes (if (bytes? jpeg)
                         jpeg
                         (file->bytes jpeg)))
  ; only one XMP block allowed in a jpeg file
  (define old-lst (get-embed-tags jpeg-bytes))
  (set-embed-jpeg! jpeg (remove-duplicates (append taglist old-lst))))

(define (add-embed-gif! gif taglist)
  (define bstr (read-gif gif))
  (define old-lst (get-embed-tags bstr))
  (set-embed-gif! gif (remove-duplicates (append taglist old-lst))))

(define (add-embed-svg! svg taglist)
  (define bstr (if (bytes? svg)
                   svg
                   (file->bytes svg)))
  (define old-lst (get-embed-tags bstr))
  (set-embed-svg! svg (remove-duplicates (append taglist old-lst))))

(define/contract (set-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(gif? img) (set-embed-gif! img taglist)]
        [(jpeg? img) (set-embed-jpeg! img taglist)]
        [(png? img) (set-embed-png! img taglist)]
        [(svg? img) (set-embed-svg! img taglist)]))

; takes a sorted list of strings and embeds them into a valid PNG
(define (set-embed-png! png taglist)
  (define png-hash (png->hash png))
  ; grab the old XMP data as an XEXPR
  (define old-xmp (get-embed-png png-hash))
  (define xexpr (if (empty? old-xmp)
                    ; if the image has no XMP data, generate some
                    (make-xmp-xexpr taglist)
                    ; change the old dc:subject xexpr
                    (set-dc:subject (string->xexpr old-xmp) taglist)))
  (define str (xexpr->xmp xexpr))
  (define itxt-bstr (make-itxt-chunk "XML:com.adobe.xmp" str))
  (define itxt-hash (make-itxt-hash itxt-bstr))
  (define new-hash (itxt-set png-hash itxt-hash "XML:com.adobe.xmp"))
  (define new-png (hash->png new-hash))
  (with-output-to-file png
    (λ ()
      (display new-png))
    #:mode 'binary
    #:exists 'truncate/replace))

; what a giant mess this is
(define (set-embed-jpeg! jpeg taglist)
  (define jpeg-bytes (file->bytes jpeg))
  (define positions (jpeg-goto-marker jpeg-bytes APP1))
  (define app1-lst (jpeg-get-app1 jpeg-bytes))
  (define app1-xmp (filter bytes?
                           (map (λ (bstr) (if (jpeg-xmp? bstr) bstr empty)) app1-lst)))
  (define filtered
    (filter pair?
            (for/list ([app1 (in-list app1-lst)]
                       [i (in-range (length app1-lst))])
              (if (jpeg-xmp? app1)
                  (list-ref positions i)
                  #f))))
  (define pos (if (empty? filtered)
                  empty
                  (car filtered)))
  (define len-bstr (if (empty? filtered)
                       #"\0"
                       (subbytes jpeg-bytes (cdr pos) (+ (cdr pos) 2))))
  (define len (bytes->number len-bstr))
  (define bstr-before (if (empty? filtered)
                          (bytes #xff SOI)
                          (subbytes jpeg-bytes 0 (car pos))))
  (define bstr-after (if (empty? filtered)
                         (subbytes jpeg-bytes 2)
                         (subbytes jpeg-bytes (+ (car pos) len 2))))
  (define xmp-str
    (cond [(empty? filtered) (xexpr->xmp (make-xmp-xexpr taglist))]
          [else
           (define bstr-before (subbytes jpeg-bytes 0 (- (car pos) 1)))
           (define xmp-str
             (bytes->string/utf-8 (subbytes (car app1-xmp) (+ 4 (bytes-length jpeg-XMP-id)))))
           (define xexpr (set-dc:subject (string->xexpr xmp-str) taglist))
           (xexpr->xmp xexpr)]))
  ; create the APP1 byte string
  (define app1-bstr
    (let ([xmp-bstr (string->bytes/utf-8 xmp-str)])
      (bytes-append (bytes #xff APP1)
                    (number->bytes (+ 2 (bytes-length xmp-bstr) (bytes-length jpeg-XMP-id)))
                    jpeg-XMP-id
                    xmp-bstr)))
  (with-output-to-file jpeg
    (λ ()
      ; sandwich the new XMP APP1 between the old data
      (printf "~a~a~a"
              bstr-before
              app1-bstr
              bstr-after))
    #:mode 'binary
    #:exists 'truncate/replace))

; embed the taglist into the gif
; application extension only available for GIF89a!
(define (set-embed-gif! img taglist)
  (define bstr (read-gif img))
  ; grab the old XMP data (if available)
  (define old-xmp (get-embed-gif bstr))
  (define xexpr (if (empty? old-xmp)
                    (make-xmp-xexpr taglist)
                    (string->xexpr (first old-xmp))))
  ; xmp string
  (define xmp-str (if (empty? old-xmp)
                      (xexpr->xmp xexpr)
                      (xexpr->xmp (set-dc:subject xexpr taglist))))
  (define new-appn-xmp (make-xmp-appn (string->bytes/utf-8 xmp-str)))
  (define appn-pos (get-appn-pos bstr))
  ; find out which one is the xmp position
  (define pos-pair
    (filter pair?
            (for/list ([pos (in-list appn-pos)])
              (if (gif-appn-xmp? (subbytes bstr (first pos) (+ (first pos) (second pos))))
                  pos
                  #f))))
  ; bytes before the XMP appn chunk
  ; make sure the format is GIF89a, not GIF87a
  (define before-bstr (if (empty? pos-pair)
                          (bytes-append #"GIF89a"
                                        (subbytes bstr 6 (- (bytes-length bstr) 1)))
                          (subbytes bstr 0 (first (first pos-pair)))))
  (define after-bstr (if (empty? pos-pair)
                         (bytes #x3b)
                         (subbytes bstr (+ (first (first pos-pair)) (second (first pos-pair))))))
  (with-output-to-file img
    (λ ()
      (printf "~a~a~a" before-bstr new-appn-xmp after-bstr))
    #:mode 'binary
    #:exists 'truncate/replace))

(define (set-embed-svg! img taglist)
  (define bstr (file->bytes img))
  (define old-xmp (get-embed-svg bstr))
  (define xexpr (if (empty? old-xmp)
                    (make-xmp-xexpr taglist)
                    (string->xexpr (first old-xmp))))
  (define xmp-str (if (empty? old-xmp)
                      (xexpr->xmp xexpr)
                      (xexpr->xmp (set-dc:subject xexpr taglist))))
  (define start (regexp-match-positions (byte-regexp #"<metadata?") bstr))
  (define end (regexp-match-positions (byte-regexp #"</metadata>") bstr))
  (define before
    (cond [start
           ; find the closing > for the <metadata
           (define close-pos
             (if start
                 (let loop ([end (cdr (car start))])
                   (if (bytes=? (subbytes bstr end (+ end 1)) #">")
                       end
                       (loop (+ end 1))))
                 #f))
           (subbytes bstr 0 (if close-pos
                                (+ close-pos 1)
                                (car (car start))))]
          [else
           (define close-tag (regexp-match-positions (byte-regexp #"</svg>") bstr))
           (subbytes bstr 0 (car (car close-tag)))]))
  (define after (if end
                    (subbytes bstr (cdr (car end)))
                    #"</svg>"))
  (define xmp-bstr
    (bytes-append before
                  (if start #"" #"<metadata>")
                  (string->bytes/utf-8 xmp-str)
                  #"</metadata>"
                  after))
  (with-output-to-file img
    (λ ()
      (display xmp-bstr))
    #:mode 'binary
    #:exists 'truncate/replace))

; retrieve the taglist from the XMP data
(define/contract (get-embed-tags img)
  (embed-support? . -> . list?)
  (define embed-xmp
    (cond [(gif? img) (get-embed-gif img)]
          [(jpeg? img) (get-embed-jpeg img)]
          [(png? img) (get-embed-png img)]
          [(svg? img) (get-embed-svg img)]))
  (cond [(empty? embed-xmp) empty]
        [else
         ; turn the XMP string into an XEXPR
         (define xexpr (string->xexpr (first embed-xmp)))
         ; find the dc:subject info
         (define dc:sub-lst (findf*-txexpr xexpr is-dc:subject?))
         (if dc:sub-lst
             ; grab the embedded tags
             (flatten (map dc:subject->list dc:sub-lst))
             empty)]))

; retrieve the XMP data located inside the iTXt block(s)
(define (get-embed-png png)
  (define png-hash (if (hash? png) png (png->hash png)))
  (define itxt-lst
    (filter string?
            (if (hash-has-key? png-hash 'iTXt)
                (map (λ (hsh)
                       (define inner (hash-ref hsh 'data))
                       (if (bytes=? #"XML:com.adobe.xmp" (hash-ref inner 'keyword))
                           ; the XMP string
                           (bytes->string/utf-8 (hash-ref inner 'text))
                           inner))
                     (hash-ref png-hash 'iTXt))
                empty)))
  (if (empty? itxt-lst)
      empty
      itxt-lst))

(define (get-embed-jpeg jpeg)
  (define jpeg-bytes (if (bytes? jpeg)
                         jpeg
                         (file->bytes jpeg)))
  ; only one XMP block allowed in a jpeg file
  (define xmp-lst (filter jpeg-xmp? (jpeg-get-app1 jpeg-bytes)))
  (if (empty? xmp-lst)
      empty
      (list
       (bytes->string/utf-8
        (subbytes (car xmp-lst) (+ 4 (bytes-length jpeg-XMP-id)))))))

; grab the taglist from the embedded XMP data
(define (get-embed-gif img)
  (define bstr (read-gif img))
  (define appn-lst (gif-get-appn bstr))
  (define filtered (filter gif-appn-xmp? appn-lst))
  (if (empty? filtered)
      empty
      (list
       (bytes->string/utf-8
        (subbytes (first filtered)
                  (+ (bytes-length gif-XMP-header) 3)
                  ; due to "magic trailer", the last 258 bytes are garbage
                  (- (bytes-length (first filtered)) 258))))))

(define (get-embed-svg svg)
  (define bstr (if (bytes? svg)
                   svg
                   (file->bytes svg)))
  (define in (open-input-bytes bstr))
  (cond [(svg-has-tag? in #"<metadata?")
         ; obtain the metadata text (sans tags)
         (define start (regexp-match-positions (byte-regexp #"<metadata?") bstr))
         ; find the closing > for the <metadata
         (define close-pos
           (cond [start
                  (let loop ([end (cdr (car start))])
                    (if (bytes=? (subbytes bstr end (+ end 1)) #">")
                        end
                        (loop (+ end 1))))]
                 [else #f]))
         (define end (regexp-match-positions (byte-regexp #"</metadata>") bstr))
         (close-input-port in)
         (list
          (bytes->string/utf-8
           (subbytes bstr (if close-pos
                              (+ close-pos 1)
                              (cdr (car start)))
                     (car (car end)))))]
        [else (close-input-port in) empty]))

; remove the tags in taglist from the image
(define/contract (remove-embed! img taglist)
  (embed-support? list? . -> . void?)
  ; get the tags from the image (if any)
  (define embed-lst (get-embed-tags img))
  (unless (empty? embed-lst)
    ; remove taglist items from embed-list
    (define new-taglist (remove* taglist embed-lst))
    (set-embed-tags! img new-taglist)))

(define ((is-tag? sym) xexpr) (and (txexpr? xexpr) (eq? sym (get-tag xexpr))))
(define is-dc:subject? (is-tag? 'dc:subject))
(define is-rdf:li? (is-tag? 'rdf:li))
(define is-rdf:Description? (is-tag? 'rdf:Description))

; take a list of tags and return a dc:subject entry
(define/contract (list->dc:subject lst)
  (list? . -> . is-dc:subject?)
  (txexpr 'dc:subject '()
          (list
           (for/fold ([bag '(rdf:Bag ())])
                     ([tag (in-list lst)])
             (append bag `((rdf:li () ,tag)))))))

; take a dc:subject entry and return a list of tags
(define/contract (dc:subject->list dc:sub)
  (is-dc:subject? . -> . list?)
  (define found (findf*-txexpr dc:sub is-rdf:li?))
  (if found
      (flatten (map (λ (item) (get-elements item)) found))
      empty))

; takes an xexpr and replaces the dc:subject entry
; with the one generated from taglist. if the xexpr
; doesn't have a dc:subject entry, or is otherwise
; incomplete, generate those bits and complete it
(define/contract (set-dc:subject xexpr taglist)
  (txexpr? list? . -> . txexpr?)
  (define new-subs (list->dc:subject taglist))
  (define-values (new-xexpr old-subs)
    (splitf-txexpr xexpr is-dc:subject? (λ (x) new-subs)))
  ; empty old-subs means it has no existing dc:subject
  (cond [(empty? old-subs)
         ; find the rdf:Description (if it has one)
         (define-values (new-txexpr rdf:description)
           (splitf-txexpr new-xexpr is-rdf:Description?))
         (define maybe-incomplete
           (if (txexpr? rdf:description)
               (append new-txexpr (list rdf:description (list new-subs)))
               (append new-xexpr
                       `((rdf:Description
                          ((rdf:about "")
                           (xmlns:Iptc4xmpCore "http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/")
                           (xmlns:dc "http://purl.org/dc/elements/1.1/")
                           (xmlns:xmp "http://ns.adobe.com/xap/1.0/")
                           (xmlns:xmpRights "http://ns.adobe.com/xap/1.0/rights/"))
                          ,new-subs)))))
         (if (eq? (get-tag maybe-incomplete) 'x:xmpmeta)
             maybe-incomplete
             (txexpr 'x:xmpmeta
                     '((x:xmptk "Ivy Image Viewer 2.0") (xmlns:x "adobe:ns:meta/"))
                     (list maybe-incomplete)))]
        [else new-xexpr]))

; take a taglist and return a complete xexpr (sans header and footer)
(define/contract (make-xmp-xexpr taglist)
  (list? . -> . txexpr?)
  (define dc:sub (list->dc:subject taglist))
  (txexpr 'x:xmpmeta
          '((x:xmptk "Ivy Image Viewer 2.0") (xmlns:x "adobe:ns:meta/"))
          `((rdf:RDF
             ((xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
             (rdf:Description
              ((rdf:about "")
               (xmlns:Iptc4xmpCore "http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/")
               (xmlns:dc "http://purl.org/dc/elements/1.1/")
               (xmlns:xmp "http://ns.adobe.com/xap/1.0/")
               (xmlns:xmpRights "http://ns.adobe.com/xap/1.0/rights/"))
              ,dc:sub)))))

; take the complete xexpr (possibly from make-xmp-xexpr) and
; return a complete xmp string with header and footer
(define/contract (xexpr->xmp xexpr)
  (txexpr? . -> . string?)
  (string-append
   ; xmp packet header
   "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>"
   ; xmp packet content
   (xexpr->string xexpr)
   ; xmp packet footer
   "<?xpacket end=\"w\"?>"))
