#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require json
         racket/dict
         racket/bool
         racket/list)
(provide (all-defined-out))

(define (path->symbol p)
  (string->symbol (path->string p)))

(define (symbol->path p)
  (string->path (symbol->string p)))

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

(define (save-dict! dct)
  (with-output-to-file master-file
    (λ () (write-json dct))
    #:exists 'truncate/replace
    #:mode 'text))

(define (clear-dict! dct)
  (for ([file-sym (in-dict-keys dct)])
    (define file-path (symbol->path file-sym))
    (printf "Checking ~s...~n" file-path)
    (unless (file-exists? file-path)
      (printf "Removing ~s from dictionary.~n" file-path)
      (dict-remove! dct file-sym))))

; enter a dictionary (master) and tag strings to search for.
; returns a list of image paths or empty on failure
(define (search-dict dct . items)
  (define search-results
    (flatten
     (for/list ([(paths tags) (in-dict dct)])
       (define result (map (λ (i) (member i tags)) items))
       ; check for false through the result list
       ; if not false, return the path for the result
       (map (λ (l) (if (false? l) l paths)) result))))
  ; filter out any false
  (define filtered (filter symbol? search-results))
  ; turn the symbols into paths and remove any duplicates
  (remove-duplicates (map symbol->path filtered)))

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
