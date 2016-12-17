#lang racket/base
; embed.rkt
; embed tags into images that support it
(require file/sha1
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
|#

; jpeg XMP string
(define XMP-id #"http://ns.adobe.com/xap/1.0/\0")
(define SOI #xd8)
(define APP1 #xe1)
(define EOI #xd9)

; takes a byte string and turns it into a decimal number
(define (bytes->number bstr)
  (string->number (bytes->hex-string bstr) 16))

; takes a decimal number and turns it into a byte string
(define (number->bytes num)
  (define str (~r num #:base 16 #:min-width 4 #:pad-string "0"))
  (hex-string->bytes str))

(define (jpeg-xmp? bstr)
  (bytes=? (subbytes bstr 4 (+ (bytes-length XMP-id) 4)) XMP-id))

(define (jpeg-has-marker? in marker-byte)
  (and (regexp-try-match (byte-regexp (bytes #xff marker-byte)) in)
       #t))

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

(define/contract (embed-support? img)
  (any/c . -> . boolean?)
  (or (png? img) (jpeg? img)))

(define/contract (add-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img) (add-embed-png! img taglist)]
        [(jpeg? img) (add-embed-jpeg! img taglist)]))

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

(define/contract (set-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img) (set-embed-png! img taglist)]
        [(jpeg? img) (set-embed-jpeg! img taglist)]))

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
             (bytes->string/utf-8 (subbytes (car app1-xmp) (+ 4 (bytes-length XMP-id)))))
           (define xexpr (set-dc:subject (string->xexpr xmp-str) taglist))
           (xexpr->xmp xexpr)]))
  ; create the APP1 byte string
  (define app1-bstr
    (let ([xmp-bstr (string->bytes/utf-8 xmp-str)])
      (bytes-append (bytes #xff APP1)
                    (number->bytes (+ 2 (bytes-length xmp-bstr) (bytes-length XMP-id)))
                    XMP-id
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

; retrieve the taglist from the XMP data
(define/contract (get-embed-tags img)
  (embed-support? . -> . list?)
  (define embed-xmp (if (png? img)
                        (get-embed-png img)
                        (get-embed-jpeg img)))
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
        (subbytes (car xmp-lst) (+ 4 (bytes-length XMP-id)))))))

; remove the tags in taglist from the image
(define/contract (remove-embed! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img)
         ; get the tags from the image (if any)
         (define embed-lst (get-embed-tags img))
         (unless (empty? embed-lst)
           ; remove taglist items from embed-list
           (define new-taglist (remove* taglist embed-lst))
           (set-embed-tags! img new-taglist))]
        [(jpeg? img) (void)]))

(define (is-dc:subject? x) (and (txexpr? x) (eq? 'dc:subject (get-tag x))))
(define (is-rdf:li? x) (and (txexpr? x) (eq? 'rdf:li (get-tag x))))

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
  (flatten
   (map (λ (item) (get-elements item))
        (findf*-txexpr dc:sub is-rdf:li?))))

; takes an xexpr and replaces the dc:subject entry
; with the one generated from taglist
(define/contract (set-dc:subject xexpr taglist)
  (txexpr? list? . -> . txexpr?)
  (define new-subs (list->dc:subject taglist))
  (define-values (new-xexpr old-subs)
    (splitf-txexpr xexpr is-dc:subject? (λ (x) new-subs)))
  new-xexpr)

; take a taglist and return a complete xexpr (sans header and footer)
(define/contract (make-xmp-xexpr taglist)
  (list? . -> . txexpr?)
  (define dc:sub (list->dc:subject taglist))
  (txexpr 'x:xmpmeta
          '((x:xmptk "XMP Core 4.4.0-Exiv2") (xmlns:x "adobe:ns:meta/"))
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
