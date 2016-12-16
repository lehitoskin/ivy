#lang racket/base
; embed.rkt
; embed tags into images that support it
(require png-image
         racket/contract
         racket/list
         racket/string
         txexpr
         xml)
(provide dc:subject->list
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
JPEG XMP keyword: #"http://ns.adobe.com/xap/1.0/\0\0"
|#

(define/contract (jpeg? img)
  (any/c . -> . boolean?)
  (define img-in (if (bytes? img)
                     (open-input-bytes img)
                     (open-input-file img)))
  (define byts (peek-bytes 2 0 img-in))
  (close-input-port img-in)
  ; #"\377\330" -> #xffd8
  (bytes=? byts #"\377\330"))

(define/contract (embed-support? img)
  (any/c . -> . boolean?)
  ;(or (png? img) (jpeg? img)))
  (png? img))

(define/contract (set-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img) (set-embed-png! img taglist)]
        [(jpeg? img) (void)]))

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

; retrieve the taglist from the XMP data
(define/contract (get-embed-tags img)
  (embed-support? . -> . list?)
  (cond [(png? img)
         (define embed-xmp (get-embed-png img))
         (cond [(empty? embed-xmp) empty]
               [else
                ; turn the XMP string into an XEXPR
                (define xexpr (string->xexpr (first embed-xmp)))
                ; find the dc:subject info
                (define dc:sub-lst (findf*-txexpr xexpr is-dc:subject?))
                (if dc:sub-lst
                    ; grab the embedded tags
                    (flatten (map dc:subject->list dc:sub-lst))
                    empty)])]
        [(jpeg? img) empty]))

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
