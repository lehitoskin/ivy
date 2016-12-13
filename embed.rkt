#lang racket/base
; embed.rkt
; embed tags into images that support it
(require jpeg/jfif
         png-image
         racket/contract
         racket/list
         racket/string)
(provide embed-support? set-embed-tags! remove-embed! get-embed-tags)

(define/contract (embed-support? img)
  (any/c . -> . boolean?)
  (or (png? img) (jfif? img)))

(define/contract (set-embed-tags! img taglist)
  (embed-support? list? . -> . void?)
  (cond [(png? img) (set-embed-png! img taglist)]
        [(jfif? img) (void)]))

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
        [(jfif? img) null]))

(define/contract (get-embed-png png)
  (png? . -> . list?)
  (define png-hash (png->hash png))
  (first
   (filter list?
           (if (hash-has-key? png-hash 'iTXt)
               (map (λ (hsh)
                      (define inner (hash-ref hsh 'data))
                      (if (bytes=? #"Tags" (hash-ref inner 'keyword))
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
        [(jfif? img) (void)]))
