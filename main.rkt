#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/class
         racket/cmdline
         racket/list
         racket/string
         "base.rkt"
         "frame.rkt")

; accept command-line path to load image
(command-line
 #:program "Ivy"
 #:usage-help
 "Calling Ivy without a path will simply open the GUI."
 "Supplying a path will tell Ivy to load the provided image."
 "Supplying multiple paths will tell Ivy to load them as a collection."
 #:once-any
 [("-o" "--search-or")
  taglist
  "Search the tags database inclusively with a comma-separated string."
  (define tags (string-split taglist ", "))
  (define search-results (sort (map path->string (search-dict master 'or tags)) string<?))
  (define len (length search-results))
  (unless (zero? len)
    (for ([sr (in-list search-results)])
      (printf "~v~n" sr)))
  (printf "Found ~a results for tags ~v~n" len tags)
  (exit)]
 [("-a" "--search-and")
  taglist
  "Search the tags database exclusively with a comma-separated string."
  (define tags (sort (string-split taglist ", ") string<?))
  (define search-results (sort (map path->string (search-dict master 'and tags)) string<?))
  (define len (length search-results))
  (unless (zero? len)
    (for ([sr (in-list search-results)])
      (printf "~v~n" sr)))
  (printf "Found ~a results for tags ~v~n" len tags)
  (exit)]
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
       (if (eq? base 'relative)
           (build-path (current-directory-for-user) name)
           rp)))
   (cond [(> (length requested-paths) 1)
          ; we want to load a collection
          (pfs checked-paths)]
         [else
          ; we want to load the image from the directory
          (define-values (base name dir?) (split-path (first checked-paths)))
          (image-dir base)
          (pfs (path-files))])
   (image-path (first checked-paths))
   (load-image (image-path) 'cmd)))

(send (ivy-canvas) focus)
(send ivy-frame show #t)
