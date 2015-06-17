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
 "Supplying multiple paths will tell Ivy to load them as a collection."
 #:args requested-images
 (unless (empty? requested-images)
   (define requested-paths
     (map (λ (img) (simplify-path (expand-user-path img)))
          requested-images))
   (define checked-paths
     (for/list ([rp requested-paths])
       ; in case the user called ivy in the same directory
       ; as the image
       (define-values (base name dir?) (split-path rp))
       (cond [(eq? base 'relative)
              (build-path (current-directory-for-user) name)]
             [else rp])))
   (cond [(> (length requested-paths) 1)
          (pfs checked-paths)]
         [else
          (define-values (base name dir?) (split-path (first checked-paths)))
          (image-dir base)
          (pfs (path-files))])
   (image-path (first checked-paths))
   (load-image (image-path) 'cmd)))

(send (ivy-canvas) focus)
(send ivy-frame show #t)
