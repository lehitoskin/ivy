#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/class
         racket/cmdline
         racket/list
         racket/path
         racket/string
         "base.rkt"
         "db.rkt"
         "frame.rkt")

(define ivy-version 1.2)

(define show-frame? (make-parameter #t))
(define tags-to-search (make-parameter empty))
(define search-type (make-parameter #f))
(define tags-to-exclude (make-parameter empty))
(define null-flag (make-parameter #f))
(define verbose? (make-parameter #f))
(define listing? (make-parameter #f))
(define adding? (make-parameter #f))
(define deleting? (make-parameter #f))
(define setting? (make-parameter #f))
(define moving? (make-parameter #f))
(define purging? (make-parameter #f))

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
 [("-V" "--version")
  "Display Ivy version."
  (printf "Ivy ~a~n" ivy-version)
  (disconnect sqlc)
  (exit)]
 [("-o" "--search-or")
  taglist
  "Search the tags database inclusively with a comma-separated string."
  (show-frame? #f)
  (search-type 'or)
  (tags-to-search
   (cond [(string-null? taglist) empty]
         [else
          (define tags
            (filter (λ (tag) (not (string-null? tag)))
                    (for/list ([tag (string-split taglist ",")])
                      (string-trim tag))))
          (remove-duplicates (sort tags string<?))]))]
 [("-a" "--search-and")
  taglist
  "Search the tags database exclusively with a comma-separated string."
  (show-frame? #f)
  (search-type 'and)
  (tags-to-search
   (cond [(string-null? taglist) empty]
         [else
          (define tags
            (filter (λ (tag) (not (string-null? tag)))
                    (for/list ([tag (string-split taglist ",")])
                      (string-trim tag))))
          (remove-duplicates (sort tags string<?))]))]
 [("-L" "--list-tags")
  "Lists the tags for the image(s)."
  (show-frame? #f)
  (listing? #t)]
 [("-A" "--add-tags")
  "Add tags to an image. ex: ivy -A \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (adding? #t)]
 [("-D" "--delete-tags")
  "Delete tags from image. ex: ivy -D \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (deleting? #t)]
 [("-P" "--purge")
  "Remove all tags from the images and purge from the database. ex: ivy -P /path/to/image ..."
  (show-frame? #f)
  (purging? #t)]
 [("-T" "--set-tags")
  "Sets the taglist of the image. ex: ivy -T \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (setting? #t)]
 [("-M" "--move-image")
  "Moves the source file(s) to the destination, updating the database."
  (show-frame? #f)
  (moving? #t)]
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
   (cond [(string-null? exclude) empty]
         [else
          (define tags
            (filter (λ (tag) (not (string-null? tag)))
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
 #:args args
 ; hijack requested-images for -M
 (unless (or (not (show-frame?))
             (empty? args))
   (define requested-paths (map relative->absolute args))
   (define checked-paths
     (for/list ([rp requested-paths])
       ; in case the user called ivy in the same directory
       ; as the image
       (define-values (base name dir?) (split-path rp))
       (if (eq? base 'relative)
           (build-path (current-directory-for-user) name)
           rp)))
   (define absolute-paths (map relative->absolute args))
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
   ; only excluding tags (resulting output may be very long!)
   [(and (empty? (tags-to-search))
         (not (empty? (tags-to-exclude))))
    (define imgs (table-column 'images 'Path))
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
                     (length exclude-sorted) (tags-to-search) (tags-to-exclude)))])]
   [(listing?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      (when (db-has-key? 'images absolute-path)
        (define img-obj (make-data-object sqlc image% absolute-path))
        (define taglist (send img-obj get-tags))
        (for ([tag (in-list taglist)])
          (if (null-flag)
              (printf "~a" (bytes-append (string->bytes/utf-8 tag) #"\0"))
              (printf "~a~n" tag)))))]
   [(adding?)
    (cond
      [(< (length args) 2)
       (raise-argument-error 'add-tags "2 or more arguments" (length args))
       (disconnect sqlc)
       (exit)]
      [else
       (define taglist (first args))
       (define imagelist (rest args))
       (for ([img (in-list imagelist)])
         (define absolute-path (path->string (relative->absolute img)))
         (define tags-to-add
           (cond [(string-null? taglist) empty]
                 [else
                  (define tags
                    (filter (λ (tag) (not (string-null? tag)))
                            (for/list ([tag (string-split taglist ",")])
                              (string-trim tag))))
                  (remove-duplicates (sort tags string<?))]))
         (unless (empty? tags-to-add)
           (define img-obj
             (if (db-has-key? 'images absolute-path)
                 (make-data-object sqlc image% absolute-path)
                 (new image% [path absolute-path])))
           (when (verbose?)
             (printf "Adding tags ~v to ~v~n" tags-to-add absolute-path))
           (add-tags! img-obj tags-to-add)))])]
   [(deleting?)
    (cond
      [(< (length args) 2)
       (raise-argument-error 'delete-tags "2 or more arguments" (length args))
       (disconnect sqlc)
       (exit)]
      [else
       (define taglist (first args))
       (define imagelist (rest args))
       (for ([img (in-list imagelist)])
         (define absolute-path (path->string (relative->absolute img)))
         (define tags-to-remove
           (cond [(string-null? taglist) empty]
                 [else
                  (define tags
                    (filter (λ (tag) (not (string-null? tag)))
                            (for/list ([tag (string-split taglist ",")])
                              (string-trim tag))))
                  (remove-duplicates (sort tags string<?))]))
         (when (and (not (empty? tags-to-remove))
                    (db-has-key? 'images absolute-path))
           (define img-obj (make-data-object sqlc image% absolute-path))
           (when (verbose?)
             (printf "Removing tags ~v from ~v~n" tags-to-remove absolute-path))
           (remove-img/tags! img-obj tags-to-remove)))])]
   [(purging?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      ; clean up the thumbnail cache a little
      (define thumb-name (path->thumb-path absolute-path))
      (when (verbose?)
        (printf "Purging ~v from the database.~n" absolute-path))
      (db-purge! absolute-path)
      ; remove the old thumbnail
      (when (file-exists? thumb-name)
        (delete-file thumb-name)))]
   [(setting?)
    (cond
      [(< (length args) 2)
       (raise-argument-error 'set-tags "2 or more arguments" (length args))
       (disconnect sqlc)
       (exit)]
      [else
       (define taglist (first args))
       (define imagelist (rest args))
       (for ([img (in-list imagelist)])
         (define absolute-path (path->string (relative->absolute img)))
         (define tags-to-set
           (cond [(string-null? taglist) empty]
                 [else
                  (define tags
                    (filter (λ (tag) (not (string-null? tag)))
                            (for/list ([tag (string-split taglist ",")])
                              (string-trim tag))))
                  (remove-duplicates (sort tags string<?))]))
         (unless (empty? tags-to-set)
           (when (verbose?)
             (printf "Setting tags of ~v to ~v~n" absolute-path tags-to-set))
           (db-set! #:threaded? #f absolute-path tags-to-set)))])]
   ; moving an image in the database to another location
   [(moving?)
    (define len (length args))
    (cond
      [(< len 2)
       (raise-argument-error 'move-image "2 or more arguments" len)
       (disconnect sqlc)
       (exit)]
      [else
       ; make sure the paths are absolute
       (define absolute-str (map (λ (ri)
                                   (path->string (relative->absolute ri)))
                                 args))
       (define dest-or-dir (last absolute-str))
       (define-values (dest-base dest-name must-be-dir?) (split-path dest-or-dir))
       (for ([old-path (in-list (take absolute-str (sub1 len)))])
         (define new-path
           (let ([file-name (file-name-from-path old-path)])
             (cond
               ; dest is a directory ending in /
               [must-be-dir? (path->string (build-path dest-base dest-name file-name))]
               ; dest is a directory that does not end in /
               [(directory-exists? (build-path dest-base dest-name))
                (path->string (build-path dest-base dest-name file-name))]
               ; dest is a file path
               [else
                (cond
                  [(> len 2)
                   (raise-argument-error 'move-image
                                         "destination as a directory when passed more than 2 args"
                                         dest-or-dir)
                   #f]
                  [else dest-or-dir])])))
         ; do the actual moving
         (when new-path
           (cond
             ; reassociate the tags to the new destination
             [(db-has-key? 'images old-path)
              ; clean up the thumbnail cache a little
              (define old-thumb-name (path->thumb-path old-path))
              (define new-thumb-name (path->thumb-path new-path))
              (define old-img-obj (make-data-object sqlc image% old-path))
              (define tags (send old-img-obj get-tags))
              ; remove the old thumbnails
              (when (file-exists? old-thumb-name)
                (delete-file old-thumb-name))
              (when (file-exists? new-thumb-name)
                (delete-file new-thumb-name))
              (db-set! #:threaded? #f new-path tags)
              (db-purge! old-path)]
             [else
              ; spit out an error message, but move anyway
              (eprintf "Database does not contain ~v~n" old-path)])
           ; copy the file over, do not overwrite dest if exists
           (when (verbose?)
             (printf "Moving ~v to ~v~n" old-path new-path))
           (rename-file-or-directory old-path new-path #f)))])])
 ; exit explicitly
 (unless (show-frame?)
   (disconnect sqlc)
   (exit)))
