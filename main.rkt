#!/usr/bin/env racket
#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require pict
         racket/class
         racket/cmdline
         racket/gui/base
         racket/list
         racket/path
         riff
         txexpr
         xml
         "base.rkt"
         "db.rkt"
         "embed.rkt"
         "error-log.rkt"
         "frame.rkt"
         "meta-editor.rkt"
         (only-in "files.rkt" ivy-version))

(define show-frame? (make-parameter #t))

(define tags-to-search (make-parameter empty))
(define search-type (make-parameter #f))
(define tags-to-exclude (make-parameter empty))
(define excluding? (make-parameter #f))

(define null-flag (make-parameter #f))
(define verbose? (make-parameter #f))
(define list-tags? (make-parameter #f))

(define add-tags? (make-parameter #f))
(define tags-to-add (make-parameter empty))

(define delete-tags? (make-parameter #f))
(define tags-to-delete (make-parameter empty))

(define set-tags? (make-parameter #f))
(define tags-to-set (make-parameter empty))

(define moving? (make-parameter #f))
(define purging? (make-parameter #f))

(define show-xmp? (make-parameter #f))
(define set-xmp? (make-parameter #f))
(define xmp-to-set (make-parameter ""))

(define show-rating? (make-parameter #f))
(define set-rating? (make-parameter #f))
(define rating-to-set (make-parameter "0"))

; make sure the path provided is a proper absolute path
(define relative->absolute (compose1 simple-form-path expand-user-path))

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
  (exit:exit)]
 [("-o" "--search-or")
  taglist
  "Search the tags database inclusively with a comma-separated string."
  (show-frame? #f)
  (search-type 'or)
  (tags-to-search (string->taglist taglist))]
 [("-a" "--search-and")
  taglist
  "Search the tags database exclusively with a comma-separated string."
  (show-frame? #f)
  (search-type 'and)
  (tags-to-search (string->taglist taglist))]
 [("-L" "--list-tags")
  "Lists the tags for the image(s)."
  (show-frame? #f)
  (list-tags? #t)]
 [("-A" "--add-tags")
  taglist
  "Add tags to an image. ex: ivy -A \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (add-tags? #t)
  (tags-to-add (string->taglist taglist))]
 [("-D" "--delete-tags")
  taglist
  "Delete tags from image. ex: ivy -D \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (delete-tags? #t)
  (tags-to-delete (string->taglist taglist))]
 [("-P" "--purge")
  "Remove all tags from the images and purge from the database. ex: ivy -P /path/to/image ..."
  (show-frame? #f)
  (purging? #t)]
 [("-T" "--set-tags")
  taglist
  "Sets the taglist of the image. ex: ivy -T \"tag0, tag1, ...\" /path/to/image ..."
  (show-frame? #f)
  (set-tags? #t)
  (tags-to-set (string->taglist taglist))]
 [("-M" "--move-image")
  "Moves the source file(s) to the destination, updating the database."
  (show-frame? #f)
  (moving? #t)]
 [("--show-xmp")
  "Extract the embedded XMP in supported images."
  (show-frame? #f)
  (show-xmp? #t)]
 [("--set-xmp")
  xmp-str
  "Set the embedded XMP in supported images."
  (show-frame? #f)
  (set-xmp? #t)
  (xmp-to-set xmp-str)]
 [("--show-rating")
  "Show the stored rating from the database."
  (show-frame? #f)
  (show-rating? #t)]
 [("--set-rating")
  rating
  "Set the xmp:Rating for the image."
  (show-frame? #f)
  (set-rating? #t)
  (rating-to-set rating)]
 #:once-each
 [("-e" "--exact-search")
  "Search the tags database for exact matches."
  (show-frame? #f)
  (exact-search? #t)]
 [("-x" "--exclude")
  exclude
  "Search the tags database with -o/-a, but exclude images with the specified tags."
  (show-frame? #f)
  (excluding? #t)
  (tags-to-exclude (string->taglist exclude))]
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
   ; if there are directories as paths, scan them
   (define absolute-paths
     (remove-duplicates
      (for/fold ([imgs empty])
                ([ap (map relative->absolute args)])
        ; directory?
        (if (directory-exists? ap)
            (append imgs (dir-files ap))
            (append imgs (list ap))))))
   (cond [(> (length absolute-paths) 1)
          ; we want to load a collection
          (pfs absolute-paths)]
         [else
          ; we want to load the image from the directory
          (define-values (base name dir?) (split-path (first absolute-paths)))
          (image-dir base)
          ; (path-files) filters only supported images
          (pfs (path-files))])
   (image-path (first absolute-paths))
   ; resize ivy-frame so that the image isn't horribly squished if it's large
   (when (supported-file? (image-path))
     ; determine the monitor dimensions
     (define-values (monitor-width monitor-height) (get-display-size))
     ; approximate canvas offset
     (define canvas-offset 84)
     (define max-width (- monitor-width canvas-offset 100))
     (define max-height (- monitor-height 100))
     (define pct (if (flif? (image-path))
                     (let ([dimensions (flif-dimensions (image-path))])
                       (rectangle (first dimensions) (second dimensions)))
                     (bitmap (image-path))))
     (define pct-width (pict-width pct))
     (define pct-height (pict-height pct))
     (cond
       ; all good, let's resize the frame
       [(and (< pct-width max-width)
             (< pct-height max-height))
        (send ivy-frame resize
              (inexact->exact (floor pct-width))
              (+ (inexact->exact (floor pct-height)) canvas-offset))]
       ; image is the same size as the monitor
       [(and (= pct-width max-width)
             (= pct-height max-height))
        (define resized (scale-to-fit pct
                                      pct-width
                                      (- max-height canvas-offset)))
        (send ivy-frame resize
              (- (inexact->exact (floor (pict-width resized))) canvas-offset)
              max-height)]
       ; wide image
       [(and (>= pct-width max-width)
             (<= pct-height max-height))
        (define y
          (cond [(> pct-height (- max-height canvas-offset))
                 max-height]
                [(<= pct-height (- max-height canvas-offset))
                 (+ pct-height canvas-offset)]
                [else pct-height]))
        (define resized (scale-to-fit pct max-width y))
        (send ivy-frame resize
              max-width
              (+ (inexact->exact (floor (pict-height resized))) canvas-offset))]
       ; tall image
       [(and (<= pct-width max-width)
             (>= pct-height max-height))
        (define resized (scale-to-fit pct
                                      pct-width
                                      (- max-height canvas-offset)))
        (send ivy-frame resize
              (inexact->exact (floor (pict-width resized)))
              max-height)]
       ; both wide and tall
       [(and (> pct-width max-width)
             (> pct-height max-height))
        (define resized (scale-to-fit pct max-width (- max-height canvas-offset)))
        (send ivy-frame resize
              (inexact->exact (floor (pict-width resized)))
              (+ (inexact->exact (floor (pict-height resized))) canvas-offset))])))
 
 (cond
   ; we aren't search for tags on the cmdline, open frame
   [(show-frame?)
    ; only change the error port if we're opening the GUI
    (current-error-port err-port)
    (send (ivy-canvas) focus)
    ; center the frame
    (send ivy-frame center 'both)
    (send ivy-frame show #t)
    ; canvas won't resize until the frame is shown.
    ; make sure we don't try to load "/" as an image.
    (unless (eq? (image-path) root-path)
      (load-image (image-path)))]
   ; only searching for tags
   [(and (search-type)
         (not (excluding?))
         (not (empty? (tags-to-search)))
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
            (printf "~a" (bytes-append (string->bytes/utf-8 sr) (bytes 0)))
            (printf "~a~n" sr)))
      (when (verbose?)
        (printf "Found ~a results for tags ~v~n" len (tags-to-search))))]
   ; only excluding tags (resulting output may be very long!)
   [(and (excluding?)
         (not (search-type))
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
            (printf "~a" (bytes-append (string->bytes/utf-8 sr) (bytes 0)))
            (printf "~a~n" sr)))
      (when (verbose?)
        (printf "Found ~a results without tags ~v~n" len (tags-to-exclude))))]
   ; searching for tags and excluding tags
   [(and (search-type)
         (excluding?)
         (not (empty? (tags-to-search)))
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
                 (printf "~a" (bytes-append (string->bytes/utf-8 ex) (bytes 0)))
                 (printf "~a~n" ex)))
           (when (verbose?)
             (printf "Found ~a results for tags ~v, excluding tags ~v~n"
                     (length exclude-sorted) (tags-to-search) (tags-to-exclude)))])]
   [(list-tags?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      (when (db-has-key? 'images absolute-path)
        (when (verbose?)
          (printf "~a:~n" absolute-path))
        (define taglist (image-taglist absolute-path))
        (for ([tag (in-list taglist)])
          (if (null-flag)
              (printf "~a" (bytes-append (string->bytes/utf-8 tag) (bytes 0)))
              (printf "~a~n" tag)))))]
   [(show-xmp?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      (when (embed-support? absolute-path)
        (when (verbose?)
          (printf "~a:~n" absolute-path))
        (define xmp (get-embed-xmp absolute-path))
        (for ([str (in-list xmp)])
          (if (null-flag)
              (printf "~a" (bytes-append (string->bytes/utf-8 str) (bytes 0)))
              (printf "~a~n" str)))))]
   ; default to 0 if there is no recorded xmp:Rating
   [(show-rating?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      (when (embed-support? absolute-path)
        (when (verbose?)
          (printf "~a: " absolute-path))
        (cond [(db-has-key? 'ratings absolute-path)
               (displayln (image-rating absolute-path))]
              [else (displayln 0)])))]
   [(add-tags?)
    (cond
      [(empty? args)
       (raise-argument-error 'add-tags "1 or more image arguments" (length args))
       (exit:exit)]
      [else
       (for ([img (in-list args)])
         (define absolute-path (path->string (relative->absolute img)))
         (unless (empty? (tags-to-add))
           (define img-obj
             (if (db-has-key? 'images absolute-path)
                 (make-data-object sqlc image% absolute-path)
                 (new image% [path absolute-path])))
           (when (verbose?)
             (printf "Adding tags ~v to ~v~n" (tags-to-add) absolute-path))
           (when (embed-support? img)
             (add-embed-tags! img (tags-to-add)))
           (add-tags! img-obj (tags-to-add))))])]
   [(delete-tags?)
    (cond
      [(empty? args)
       (raise-argument-error 'delete-tags "1 or more image arguments" (length args))
       (exit:exit)]
      [else
       (for ([img (in-list args)])
         (define absolute-path (path->string (relative->absolute img)))
         (when (and (not (empty? (tags-to-delete)))
                    (db-has-key? 'images absolute-path))
           (define img-obj (make-data-object sqlc image% absolute-path))
           (when (verbose?)
             (printf "Removing tags ~v from ~v~n" (tags-to-delete) absolute-path))
           (when (embed-support? img)
             (del-embed-tags! img (tags-to-delete)))
           (del-tags! img-obj (tags-to-delete))))])]
   [(purging?)
    (for ([img (in-list args)])
      (define absolute-path (path->string (relative->absolute img)))
      ; clean up the thumbnail cache a little
      (define thumb-name (path->md5 absolute-path))
      (when (verbose?)
        (printf "Purging ~v from the database.~n" absolute-path))
      (db-purge! absolute-path)
      ; remove the old thumbnail
      (when (file-exists? thumb-name)
        (delete-file thumb-name)))]
   [(set-tags?)
    (cond
      [(empty? args)
       (raise-argument-error 'set-tags "1 or more image arguments" (length args))
       (exit:exit)]
      [else
       (for ([img (in-list args)])
         (define absolute-path (path->string (relative->absolute img)))
         (unless (empty? (tags-to-set))
           (when (verbose?)
             (printf "Setting tags of ~v to ~v~n" absolute-path (tags-to-set)))
           (reconcile-tags! absolute-path (tags-to-set))
           (when (embed-support? absolute-path)
             (set-embed-tags! absolute-path (tags-to-set)))))])]
   ; set the XMP metadata for a file
   [(set-xmp?)
    (cond
      [(empty? args)
       (raise-argument-error 'set-xmp "1 or more image arguments" (length args))
       (exit:exit)]
      [else
       ; make sure the paths are absolute
       (define absolutes (map relative->absolute args))
       (for ([path (in-list absolutes)])
         (when (embed-support? path)
           ; set the XMP data
           (set-embed-xmp! path (xmp-to-set))
           ; grab the tag list and update the database
           (define xexpr (string->xexpr (xmp-to-set)))
           ; find the dc:subject info
           (define dc:sub-lst (findf*-txexpr xexpr is-dc:subject?))
           (define tags
             (if dc:sub-lst
                 ; grab the embedded tags
                 (flatten (map dc:subject->list dc:sub-lst))
                 empty))
           (when (verbose?)
             (printf "Setting the XMP of ~a...~n" path))
           (reconcile-tags! (path->string path) (sort tags string<?))))])]
   ; set the xmp:Rating in both the database and the XMP
   [(set-rating?)
    (cond
      [(empty? args)
       (raise-argument-error 'add-tags "1 or more image arguments" (length args))
       (exit:exit)]
      [else
       (for ([img (in-list args)])
         (define absolute-path (path->string (relative->absolute img)))
         (when (verbose?)
           (printf "Setting the rating for ~a...~n" absolute-path))
         ; set the rating in the database
         (set-image-rating! absolute-path (string->number (rating-to-set)))
         ; set the rating in the embedded XMP
         (when (embed-support? absolute-path)
           (define xmp (get-embed-xmp absolute-path))
           (define xexpr (if (empty? xmp)
                             (make-xmp-xexpr empty)
                             (string->xexpr (first xmp))))
           (define tag (findf-txexpr xexpr (is-tag? 'xmp:Rating)))
           (define setted
             ((set-xmp-tag (if tag 'xmp:Rating 'rdf:Description))
              xexpr
              (create-dc-meta "xmp:Rating"
                              (rating-to-set)
                              ""
                              (box (list (xexpr->string xexpr))))))
           (set-embed-xmp! absolute-path (xexpr->string setted))))])]
   ; moving an image in the database to another location
   [(moving?)
    (define len (length args))
    (cond
      [(< len 2)
       (raise-argument-error 'move-image "2 or more arguments" len)
       (exit:exit)]
      [else
       ; make sure the paths are absolute
       (define absolute-str
         (for/list ([ri (in-list args)])
           (path->string (relative->absolute ri))))
       (define dest-or-dir (last absolute-str))
       (define-values (dest-base dest-name must-be-dir?) (split-path dest-or-dir))
       (for ([old-path (in-list (take absolute-str (- len 1)))])
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
                                         "destination needs to be a directory"
                                         dest-or-dir)
                   #f]
                  [else dest-or-dir])])))
         ; do the actual moving
         (when new-path
           (cond
             ; reassociate the tags to the new destination
             [(db-has-key? 'images old-path)
              ; clean up the thumbnail cache a little
              (define old-thumb-name (path->md5 old-path))
              (define new-thumb-name (path->md5 new-path))
              (define old-img-obj (make-data-object sqlc image% old-path))
              (define tags (send old-img-obj get-tags))
              ; remove the old thumbnails
              (when (file-exists? old-thumb-name)
                (delete-file old-thumb-name))
              (when (file-exists? new-thumb-name)
                (delete-file new-thumb-name))
              (reconcile-tags! new-path tags)
              ; preserve the old rating, if we can
              (define old-rating
                (if (db-has-key? 'ratings old-path)
                    (image-rating old-path)
                    0))
              (set-image-rating! new-path old-rating)
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
   (exit:exit)))
