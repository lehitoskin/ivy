#lang racket/base
; embed.rkt
; embed tags into images that support it
(require png-image
         racket/contract
         racket/list
         racket/string
         txexpr)
(provide dc:subject->list
         embed-support?
         get-embed-tags
         list->dc:subject
         remove-embed!
         set-dc:subject
         set-embed-tags!
         xexpr->xmp)

#|
xmp packet comments
#"<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>"
payload goes here
#"<?xpacket end=\"w\"?>"

PNG XMP keyword: #"XML:com.adobe.xmp"
JPEG XMP keyword: #"http://ns.adobe.com/xap/1.0/\0\0"
|#

(define (jpeg? img)
  (define img-in (if (bytes? img)
                     (open-input-bytes img)
                     (open-input-file img)))
  (define byts (peek-bytes img-in 2 0 img-in))
  (close-input-port img-in)
  ; #"\377\330" -> #xffd8
  (bytes=? byts #"\377\330"))

(define/contract (embed-support? img)
  (any/c . -> . boolean?)
  (or (png? img) (jpeg? img)))

(define/contract (set-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img) (set-embed-png! img taglist)]
        [(jpeg? img) (void)]))

; takes a sorted list of strings and embeds them into a valid PNG
(define/contract (set-embed-png! png taglist)
  ((and/c png? path-string?) (listof string?) . -> . void?)
  (define str (string-join taglist ","))
  (define png-hash (png->hash png))
  (define itxt-bstr (make-itxt-chunk "Tags" str))
  (define itxt-hash (make-itxt-hash itxt-bstr))
  (define new-hash (itxt-set png-hash itxt-hash "Tags"))
  (define new-png (hash->png new-hash))
  (with-output-to-file png
    (λ ()
      (display new-png))
    #:mode 'binary
    #:exists 'truncate/replace))

(define/contract (get-embed-tags img)
  (embed-support? . -> . list?)
  (cond [(png? img) (get-embed-png img)]
        [(jpeg? img) null]))

(define/contract (get-embed-png png)
  (png? . -> . list?)
  (define png-hash (png->hash png))
  (first
   (filter list?
           (if (hash-has-key? png-hash 'iTXt)
               (map (λ (hsh)
                      (define inner (hash-ref hsh 'data))
                      (if (bytes=? #"XML:com.adobe.xmp" (hash-ref inner 'keyword))
                          (string-split (bytes->string/utf-8 (hash-ref inner 'text)) ",")
                          inner))
                    (hash-ref png-hash 'iTXt))
               null))))

; remove the tags in taglist from the image
(define/contract (remove-embed! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img)
         (define old-tags (get-embed-tags img))
         (unless (null? old-tags)
           (define removed (remove* taglist old-tags))
           (set-embed-png! img removed))]
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
