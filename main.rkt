#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/bool
         racket/class
         racket/cmdline
         racket/dict
         racket/format
         racket/list
         racket/path
         racket/string
         "base.rkt"
         "db.rkt"
         "frame.rkt")

(define show-frame? (make-parameter #t))
(define tags-to-search (make-parameter empty))
(define search-type (make-parameter #f))
(define tags-to-exclude (make-parameter empty))
(define null-flag (make-parameter #f))
(define verbose? (make-parameter #f))

; make sure the path provided is a proper absolute path
(define (relative->absolute path)
  (simple-form-path (expand-user-path path)))

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
  "Display verbose information for certain operations."
  (show-frame? #f)
  (verbose? #t)]
 #:multi
 [("-L" "--list-tags")
  img
  "Lists the tags for the image."
  (show-frame? #f)
  (define absolute-path (path->string (relative->absolute img)))
  (when (db-has-key? "images" absolute-path)
    (define img-obj (make-data-object sqlc image% absolute-path))
    (define taglist (send img-obj get-tags))
    (for ([tag (in-list taglist)])
      (if (null-flag)
          (printf "~a" (bytes-append (string->bytes/utf-8 tag) #"\0"))
          (printf "~a~n" tag))))]
 [("-A" "--add-tags")
  taglist img
  "Add tags to an image. ex: ivy -A \"tag0, tag1, ...\" /path/to/image"
  (show-frame? #f)
  (define absolute-path (path->string (relative->absolute img)))
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
      (if (db-has-key? "images" absolute-path)
          (make-data-object sqlc image% absolute-path)
          (new image% [path absolute-path])))
    (when (verbose?)
      (printf "Adding tags ~v to ~v~n" tags-to-add absolute-path))
    (add-tags! img-obj tags-to-add))]
 [("-D" "--delete-tags")
  taglist img
  "Delete tags from image. ex: ivy -D \"tag0, tag1, ...\" /path/to/image"
  (show-frame? #f)
  (define absolute-path (path->string (relative->absolute img)))
  (define tags-to-remove
    (cond [(string=? taglist "") empty]
          [else
           (define tags
             (filter (λ (tag) (not (string=? tag "")))
                     (for/list ([tag (string-split taglist ",")])
                       (string-trim tag))))
           (remove-duplicates (sort tags string<?))]))
  (when (and (not (empty? tags-to-remove))
             (db-has-key? "images" absolute-path))
    (define img-obj (make-data-object sqlc image% absolute-path))
    (when (verbose?)
      (printf "Removing tags ~v from ~v~n" tags-to-remove absolute-path))
    (remove-tags! sqlc img-obj tags-to-remove))]
 [("-T" "--set-tags")
  taglist img
  "Sets the taglist of the image. ex: ivy -T \"tag0, tag1, ...\" /path/to/image"
  (show-frame? #f)
  (define absolute-path (path->string (relative->absolute img)))
  (define tags-to-set
    (cond [(string=? taglist "") empty]
          [else
           (define tags
             (filter (λ (tag) (not (string=? tag "")))
                     (for/list ([tag (string-split taglist ",")])
                       (string-trim tag))))
           (remove-duplicates (sort tags string<?))]))
  (unless (empty? tags-to-set)
    (when (verbose?)
      (printf "Setting tags of ~v to ~v~n" absolute-path tags-to-set))
    (db-set! #:threaded? #f absolute-path tags-to-set))]
 [("-M" "--move-image")
  source dest
  "Moves the source file to the destination, updating the database."
  (show-frame? #f)
  ; make sure the paths are absolute
  (define old-path (path->string (relative->absolute source)))
  (define absolute-dest (relative->absolute dest))
  (define new-path
    (let-values ([(base new-name must-be-dir?) (split-path absolute-dest)])
      (define file-name (file-name-from-path old-path))
      (cond
        ; dest is a directory ending in /
        [must-be-dir? (build-path base new-name file-name)]
        ; dest is a directory that does not end in /
        [(directory-exists? (build-path base new-name)) (build-path base new-name file-name)]
        ; dest is a file path
        [else (path->string absolute-dest)])))
  (when (db-has-key? "images" old-path)
    (define old-img-obj (make-data-object sqlc image% old-path))
    (define tags (send old-img-obj get-tags))
    (db-set! #:threaded? #f new-path tags)
    ; copy the file over, overwrite dest if exists
    (when (verbose?)
      (printf "Moving ~a to ~a~n" old-path new-path))
    (rename-file-or-directory old-path new-path #t)
    (clean-db!))]
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
   (define absolute-paths (map relative->absolute requested-images))
   (cond [(> (length absolute-paths) 1)
          ; we want to load a collection
          (pfs absolute-paths)]
         [else
          ; we want to load the image from the directory
          (define-values (base name dir?) (split-path (first absolute-paths)))
          (image-dir base)
          (pfs (path-files))])
   (image-path (first absolute-paths))
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
            (printf "~a~n" sr)))
      (when (verbose?)
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
            (printf "~a~n" sr)))
      (when (verbose?)
        (printf "Found ~a results without tags ~v~n" len (tags-to-exclude))))]
   ; searching for tags and excluding tags
   [(and (not (empty? (tags-to-search)))
         (not (empty? (tags-to-exclude))))
    (define search-results
      (if (exact-search?)
          (search-db-exact (search-type) (tags-to-search))
          (search-db-inexact (search-type) (tags-to-search))))
    (cond [(zero? (length search-results))
           (when (verbose?)
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
                 (printf "~a~n" ex)))
           (when (verbose?)
             (printf "Found ~a results for tags ~v, excluding tags ~v~n"
                     (length exclude-sorted) (tags-to-search) (tags-to-exclude)))])]))
