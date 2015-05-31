#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/class
         racket/cmdline
         racket/list
         "base.rkt"
         "frame.rkt")

; accept command-line path to load image
(command-line
 #:program "Ivy"
 #:usage-help
 "Calling Ivy without a path will simply open the GUI."
 "Supplying a path will tell Ivy to load the provided image."
 #:args requested-image
 (unless (empty? requested-image)
   (define requested-path (string->path (first requested-image)))
   (if (relative-path? requested-path)
       (image-path (build-path (current-directory-for-user) requested-path))
       (image-path requested-path))
   (load-image (image-path))))

(send ivy-frame show #t)
