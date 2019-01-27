#lang racket/base
; config.rkt
(require json "files.rkt")
(provide (all-defined-out))

(define config-hash
  (if (file-exists? config-file)
      (let ([in (open-input-file config-file)])
        (define js (read-json in))
        (close-input-port in)
        (hash-copy js))
      (make-hasheq
       '((animation? . #f)
         (browse-regex? . #f)
         (search-exact? . #f)
         (search-type . "or")))))

(define (save-config)
  (let ([out (open-output-file config-file
                               #:mode 'binary
                               #:exists 'truncate/replace)])
    (write-json config-hash out)
    (close-output-port out)))
