#!/usr/bin/env racket
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
   ; absolute path to the image
   (define requested-path
     (simplify-path (expand-user-path (first requested-image))))
   (define-values (base name dir?) (split-path requested-path))
   ; in case the user called ivy in the same directory as the image
   ; e.g. `ivy batman.jpg'
   (cond [(eq? base 'relative)
          (define path (build-path (current-directory-for-user) name))
          (define-values (base-dir img-name dir?) (split-path path))
          (image-path path)
          (image-dir base-dir)]
         [else
          (image-path requested-path)
          (image-dir base)])
   (pfs (path-files))
   (load-image (image-path) 'cmd)))

(send (ivy-canvas) focus)
(send ivy-frame show #t)
