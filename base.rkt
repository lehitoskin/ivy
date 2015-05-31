#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require json racket/dict)
(provide (all-defined-out))

; master dictionary
; (absolute-file-path . '(sorted list of tags))
(define master (make-hash))
(define ivy-path (cond [(eq? (system-type) 'unix)
                        (build-path (find-system-path 'home-dir)
                                    ".config/ivy")]
                       [(eq? (system-type) 'windows)
                        (normal-case-path
                         (build-path (find-system-path 'home-dir)
                                     "appdata/local/ivy"))]
                       [(eq? (system-type) 'macosx)
                        (build-path (find-system-path 'home-dir)
                                    "Library/Application Support/ivy")]))
(define master-file (build-path ivy-path "catalog.json"))

(define (save-dict dct)
  (with-output-to-file master-file
    (λ () (write-json dct))
    #:exists 'truncate/replace
    #:mode 'text))

; create the config directory
(unless (directory-exists? ivy-path)
  (make-directory ivy-path))

; load the dictionary file
; this could get very big!
(when (file-exists? master-file)
  (define json-port (open-input-file master-file))
  (define dict-json (read-json json-port))
  (for ([(k v) (in-dict dict-json)])
    (dict-set! master k v))
  (close-input-port json-port))
