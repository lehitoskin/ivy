#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/bool
         racket/class
         racket/cmdline
         racket/dict
         racket/list
         racket/string
         "base.rkt"
         "db.rkt"
         "frame.rkt")

(define show-frame? (make-parameter #t))
(define tags-to-search (make-parameter empty))
(define search-type (make-parameter #f))
(define tags-to-exclude (make-parameter empty))
(define null-flag (make-parameter #f))
(define verbose-search (make-parameter #f))

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
  (show-frame? #f)
  (search-type 'or)
  (tags-to-search
   (cond [(string=? taglist "") empty]
         [else
          (define tags
            (filter (λ (tag) (not (string=? tag "")))
                    (for/list ([tag (string-split taglist ",")])
                      (string-trim tag))))
          (remove-duplicates (sort tags string<?))]))]
 [("-a" "--search-and")
  taglist
  "Search the tags database exclusively with a comma-separated string."
  (show-frame? #f)
  (search-type 'and)
  (tags-to-search
   (cond [(string=? taglist "") empty]
         [else
          (define tags
            (filter (λ (tag) (not (string=? tag "")))
                    (for/list ([tag (string-split taglist ",")])
                      (string-trim tag))))
          (remove-duplicates (sort tags string<?))]))]
 #:once-each
 [("-e" "--exact-search")
  "Search the tags database for exact matches."
  (show-frame? #f)
  (exact-search? #t)]
 [("-x" "--exclude")
  exclude
  "Search the tags database with -o/-a, but exclude images with the specified tags."
  (show-frame? #f)
  (tags-to-exclude
   (cond [(string=? exclude "") empty]
         [else
          (define tags
            (filter (λ (tag) (not (string=? tag "")))
                    (for/list ([tag (string-split exclude ",")])
                      (string-trim tag))))
          (remove-duplicates (sort tags string<?))]))]
 [("-n" "--null")
  "Search result items are terminated by a null character instead of by whitespace."
  (show-frame? #f)
  (null-flag #t)]
 [("-v" "--verbose")
  "Display search results summary after the list of results."
  (show-frame? #f)
  (verbose-search #t)]
 #:multi
 [("-A" "--add-tags")
  taglist img
  "Add tags to an image. ex: ivy -A \"tag0, tag1, ...\" /path/to/image"
  (show-frame? #f)
  (define tags-to-add
    (cond [(string=? taglist "") empty]
          [else
           (define tags
             (filter (λ (tag) (not (string=? tag "")))
                     (for/list ([tag (string-split taglist ",")])
                       (string-trim tag))))
           (remove-duplicates (sort tags string<?))]))
  (unless (empty? tags-to-add)
    (define img-obj
      (if (db-has-key? "images" img)
          (make-data-object sqlc image% img)
          (new image% [path img])))
    (printf "Adding tags ~v to ~v~n" tags-to-add img)
    (add-tags! img-obj tags-to-add))]
 [("-D" "--delete-tags")
  taglist img
  "Delete tags from image. ex: ivy -D \"tag0, tag1, ...\" /path/to/image"
  (show-frame? #f)
  (define tags-to-remove
    (cond [(string=? taglist "") empty]
          [else
           (define tags
             (filter (λ (tag) (not (string=? tag "")))
                     (for/list ([tag (string-split taglist ",")])
                       (string-trim tag))))
           (remove-duplicates (sort tags string<?))]))
  (when (and (not (empty? tags-to-remove))
             (db-has-key? "images" img))
    (define img-obj (make-data-object sqlc image% img))
    (printf "Removing tags ~v from ~v~n" tags-to-remove img)
    (remove-tags! sqlc img-obj tags-to-remove))]
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
 
 (cond
   ; we aren't search for tags on the cmdline, open frame
   [(show-frame?)
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
        (if (null-flag)
            (printf "~a" (bytes-append (string->bytes/utf-8 sr) #"\0"))
            (printf "~v~n" sr)))
      (when (verbose-search)
        (printf "Found ~a results for tags ~v~n" len (tags-to-search))))]
   ; only excluding tags (resulting output may be very big!)
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
        (if (null-flag)
            (printf "~a" (bytes-append (string->bytes/utf-8 sr) #"\0"))
            (printf "~v~n" sr)))
      (when (verbose-search)
        (printf "Found ~a results without tags ~v~n" len (tags-to-exclude))))]
   ; searching for tags and excluding tags
   [(and (not (empty? (tags-to-search)))
         (not (empty? (tags-to-exclude))))
    (define search-results
      (if (exact-search?)
          (search-db-exact (search-type) (tags-to-search))
          (search-db-inexact (search-type) (tags-to-search))))
    (cond [(zero? (length search-results))
           (when (verbose-search)
             (printf "Found 0 results for tags ~v~n" (tags-to-search)))]
          [else
           (define exclude
             (if (exact-search?)
                 (exclude-search-exact search-results (tags-to-exclude))
                 (exclude-search-inexact search-results (tags-to-exclude))))
           (define exclude-sorted (sort (map path->string exclude) string<?))
           (for ([ex (in-list exclude-sorted)])
             (if (null-flag)
                 (printf "~a" (bytes-append (string->bytes/utf-8 ex) #"\0"))
                 (printf "~v~n" ex)))
           (when (verbose-search)
             (printf "Found ~a results for tags ~v, excluding tags ~v~n"
                     (length exclude-sorted) (tags-to-search) (tags-to-exclude)))])]))
