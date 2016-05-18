#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/class
         racket/cmdline
         racket/dict
         racket/list
         racket/string
         "base.rkt"
         "db.rkt"
         "frame.rkt")

(define tags-to-search (make-parameter empty))
(define search-type (make-parameter #f))
(define tags-to-exclude (make-parameter empty))
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
  (search-type 'or)
  (tags-to-search (string-split taglist ", "))]
 [("-a" "--search-and")
  taglist
  "Search the tags database exclusively with a comma-separated string."
  (search-type 'and)
  (tags-to-search (sort (string-split taglist ", ") string<?))]
 #:once-each
 [("-e" "--exact-search")
  "Search the tags database for exact matches."
  (exact-search? #t)]
 [("-x" "--exclude")
  exclude
  "Search the tags database with -o/-a, but exclude images with the specified tags."
  (tags-to-exclude (string-split exclude ", "))]
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
   (load-image (image-path) 'cmd))
 ; we aren't search for tags on the cmdline, open frame
 (cond [(and (empty? (tags-to-search))
             (empty? (tags-to-exclude)))
        (send (ivy-canvas) focus)
        (send ivy-frame show #t)]
       ; only searching for tags
       [(and (not (empty? (tags-to-search)))
             (empty? (tags-to-exclude)))
        (define search-results
          (if (exact-search?)
              (search-db-exact (search-type) (tags-to-search))
              (search-db-inexact (search-type) (tags-to-search))))
        (define search-sorted (sort (map path->string search-results) string<?))
        (define len (length search-sorted))
        (unless (zero? len)
          (for ([sr (in-list search-sorted)])
            (printf "~v~n" sr))
          (printf "Found ~a results for tags ~v~n" len (tags-to-search)))]
       ; only excluding tags
       [(and (empty? (tags-to-search))
             (not (empty? (tags-to-exclude))))
        (define imgs (table-column "images" "Path"))
        (define excluded
          (if (exact-search?)
              (exclude-search-exact imgs (tags-to-exclude))
              (exclude-search-inexact imgs (tags-to-exclude))))
        (define final-sorted (sort (map path->string excluded) string<?))
        (define len (length final-sorted))
        (unless (zero? len)
          (for ([sr (in-list final-sorted)])
            (printf "~v~n" sr))
          (printf "Found ~a results without tags ~v~n" len (tags-to-exclude)))]
       ; searching for tags and excluding tags
       [(and (not (empty? (tags-to-search)))
             (not (empty? (tags-to-exclude))))
        (define search-results
          (if (exact-search?)
              (search-db-exact (search-type) (tags-to-search))
              (search-db-inexact (search-type) (tags-to-search))))
        (cond [(zero? (length search-results))
               (printf "Found 0 results for tags ~v~n" (tags-to-search))]
              [else
               (define exclude
                 (if (exact-search?)
                     (exclude-search-exact search-results (tags-to-exclude))
                     (exclude-search-inexact search-results (tags-to-exclude))))
               (define exclude-sorted (sort (map path->string exclude) string<?))
               (for ([ex (in-list exclude-sorted)])
                 (printf "~v~n" ex))
               (printf "Found ~a results for tags ~v, excluding tags ~v~n"
                       (length exclude-sorted) (tags-to-search) (tags-to-exclude))])]))
