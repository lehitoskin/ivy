#lang racket/base
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require framework
         images/flomap
         pict
         racket/bool
         racket/class
         racket/gui/base
         racket/list
         racket/math
         racket/string
         riff
         txexpr
         xml
         "base.rkt"
         "db.rkt"
         "db-statistics.rkt"
         "embed.rkt"
         "error-log.rkt"
         "files.rkt"
         "meta-editor.rkt"
         "search-dialog.rkt"
         "tag-browser.rkt")
(provide (all-defined-out) exit:exit)

; framework stuff
(application:current-app-name "Ivy")
(application-quit-handler exit:exit)
(void
 (exit:insert-on-callback
  (λ ()
    ; kill the animation thread, if applicable
    (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
      (kill-thread (animation-thread)))
    ; wait for any xmp threads to finish before exiting
    (unless (hash-empty? xmp-threads)
      (for ([pair (in-hash-pairs xmp-threads)])
        (printf "Waiting for thread ~a to finish...~n" (car pair))
        (sync (cdr pair))))
    ; clean up the decoder pointer
    (when (decoder)
      (flif-abort-decoder! (decoder))
      (flif-destroy-decoder! (decoder))
      (decoder #f))
    (disconnect sqlc))))

(define closer-frame%
  (class frame%
    (super-new)
    ; clean up and exit the program
    (define (on-close) (exit:exit))
    (augment on-close)))

(define ivy-frame
  (new closer-frame%
       [label "Ivy Image Viewer"]
       [style '(fullscreen-button)]
       [width 800]
       [height 600]))

; set the icon for the frame
(unless macosx?
  (void (send ivy-frame set-icon logo-bmp)))

(define ivy-menu-bar
  (new menu-bar%
       [parent ivy-frame]))

(define ivy-menu-bar-file
  (new menu%
       [parent ivy-menu-bar]
       [label "&File"]))

(define ivy-menu-bar-edit
  (new menu%
       [parent ivy-menu-bar]
       [label "&Edit"]))

(define ivy-menu-bar-navigation
  (new menu%
       [parent ivy-menu-bar]
       [label "&Navigation"]))

(define ivy-menu-bar-view
  (new menu%
       [parent ivy-menu-bar]
       [label "&View"]))

(define ivy-menu-bar-window
  (when macosx?
    (new menu%
         [parent ivy-menu-bar]
         [label "&Window"])))

(define ivy-menu-bar-help
  (new menu%
       [parent ivy-menu-bar]
       [label "&Help"]))

;; Fullscreen handling ;;

(define (toggle-fullscreen canvas frame)
  (define was-fullscreen?  (send frame is-fullscreened?))
  (define going-to-be-fullscreen? (not was-fullscreen?))
  (send frame fullscreen going-to-be-fullscreen?)
  (unless macosx?
    (on-fullscreen-event going-to-be-fullscreen?)))

(define (on-fullscreen-event is-fullscreen?)
  (cond [is-fullscreen?
         (remove-children ivy-frame (list ivy-toolbar-hpanel status-bar-hpanel))]
        [else
         (send ivy-frame delete-child (ivy-canvas))
         (add-children ivy-frame (list ivy-toolbar-hpanel (ivy-canvas) status-bar-hpanel))])
  (send ivy-frame reflow-container)
  (send (ivy-canvas) focus))

; polling timer callback; only way to know the user is fullscreen if they don't
; use our ui callback, e.g. fullscreen button on mac; only be relevant on OS X?
(when macosx?
  (define was-fullscreen? (make-parameter #f))
  (define ivy-fullscreen-poller
    (new timer%
         [interval 100]
         [notify-callback (λ ()
                            (define is-fullscreen? (send ivy-frame is-fullscreened?))
                            (cond [(not (eq? (was-fullscreen?) is-fullscreen?))
                                   (on-fullscreen-event is-fullscreen?)
                                   (was-fullscreen? is-fullscreen?)]))]))
  (let ([default-handler (application-quit-handler)])
    (application-quit-handler
     (λ ()
       (send ivy-fullscreen-poller stop)
       (default-handler)))))

;; File menu items ;;

; eliminate Gtk-Message message errors
(define open-dialog
  (new dialog%
       [label "Ivy - Choose A File"]))

; opening a single image will have the current directory
; contents be the collection
(define ivy-menu-bar-file-open
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Open new collection"]
       [shortcut #\O]
       [help-string "Open a file or files to view."]
       [callback
        (λ (i e)
          (define paths
            (get-file-list
             "Select an image or images to view."
             open-dialog
             (image-dir)
             #f
             #f
             null
             `(("All images"
                ,(string-append
                  "*"
                  (string-join +supported-extensions+ ";*")))
               ("Any" "*.*"))))
          ; make sure the path is not false
          (when paths
            (define img-path (first paths))
            (cond [(> (length paths) 1) (pfs paths)]
                  [else
                   (define-values (base name dir?) (split-path img-path))
                   (image-dir base)
                   (pfs (path-files base))])
            (send (ivy-tag-tfield) set-field-background color-white)
            (image-path img-path)
            (collect-garbage 'incremental)
            (load-image img-path)))]))

(define ivy-menu-bar-file-append
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "Append images to c&ollection"]
       [shortcut #\O]
       [shortcut-prefix (if macosx? '(cmd shift) '(ctl shift))]
       [help-string "Append images to existing collection"]
       [callback
        (λ (i e)
          (define paths
            (get-file-list
             "Select an image or images to view."
             open-dialog
             (image-dir)
             #f
             #f
             null
             `(("All images"
                ,(string-append
                  "*"
                  (string-join +supported-extensions+ ";*")))
               ("Any" "*.*"))))
          ; the user did not click cancel
          (when paths
            (cond
              ; empty collection, adding images and load the first in the list
              [(equal? (first (pfs)) +root-path+)
               (define img-path (first paths))
               (define-values (base name dir?) (split-path img-path))
               (image-dir base)
               (pfs paths)
               (image-path img-path)
               (collect-garbage 'incremental)
               (load-image img-path)]
              ; collection has images; appending to collection
              [else
               ; no duplicate paths allowed!
               (pfs (remove-duplicates (append (pfs) paths)))
               ; change label because it usually isn't called until
               ; (load-image) is called and we want to see the changes now
               (send (status-bar-position) set-label
                     (format "~a / ~a"
                             (+ (get-index (image-path) (pfs)) 1)
                             (length (pfs))))])))]))

(define ivy-menu-bar-dir-open
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "Open directory"]
       [help-string "Open a directory to view."]
       [callback
        (λ (i e)
          (define dir
            (get-directory
             "Select a directory to view."
             open-dialog
             (image-dir)))
          ; make sure dir is not false
          (when dir
            (define paths (dir-files dir))
            ; do nothing if dir doesn't contain any supported images
            (unless (empty? paths)
              (define img-path (first paths))
              (cond [(> (length paths) 1) (pfs paths)]
                    [else
                     (define-values (base name dir?) (split-path img-path))
                     (image-dir base)
                     (pfs (path-files base))])
              (send (ivy-tag-tfield) set-field-background color-white)
              (image-path img-path)
              (collect-garbage 'incremental)
              (load-image img-path))))]))

(define ivy-menu-bar-dir-append
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "Append directory"]
       [help-string "Append a directory to the current collection."]
       [callback
        (λ (i e)
          (define dir
            (get-directory
             "Select a directory to append."
             open-dialog
             (image-dir)))
          ; make sure dir is not false
          (when dir
            (define paths (dir-files dir))
            ; do nothing if dir doesn't contain any supported images
            (unless (empty? paths)
              (cond
                ; empty collection, adding images and load the first in the list
                [(equal? (first (pfs)) +root-path+)
                 (define img-path (first paths))
                 (define-values (base name dir?) (split-path img-path))
                 (image-dir base)
                 (pfs paths)
                 (image-path img-path)
                 (collect-garbage 'incremental)
                 (load-image img-path)]
                ; collection has images; appending to collection
                [else
                 ; no duplicate paths allowed!
                 (pfs (remove-duplicates (append (pfs) paths)))
                 ; change label because it usually isn't called until
                 ; (load-image) is called and we want to see the changes now
                 (send (status-bar-position) set-label
                       (format "~a / ~a"
                               (+ (get-index (image-path) (pfs)) 1)
                               (length (pfs))))]))))]))

; reset the GUI to defaults
(define ivy-menu-bar-file-collection-new
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&New collection"]
       [shortcut #\N]
       [help-string "Empties the current collection"]
       [callback
        (λ (i e)
          (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
            (kill-thread (animation-thread)))
          #;(unless (or (false? (decoder-thread)) (thread-dead? (decoder-thread)))
            (kill-thread (decoder-thread))
            (displayln "New collection; aborting decoder...")
            (flif-abort-decoder! (decoder))
            (display "Destroying decoder... ")
            (flif-destroy-decoder! (decoder))
            (displayln "done")
            (decoder #f))
          (when (decoder)
            (flif-abort-decoder! (decoder))
            (flif-destroy-decoder! (decoder))
            (decoder #f))
          (image-dir (find-system-path 'home-dir))
          (pfs (list +root-path+))
          (image-path +root-path+)
          (send (ivy-canvas) set-on-paint!
                (λ (canvas dc)
                  (send canvas set-canvas-background color-black)))
          (send (ivy-canvas) refresh)
          (send ivy-frame set-label "Ivy Image Viewer")
          (send (ivy-actions-rating) set-string-selection "0 🌟")
          (send (status-bar-position) set-label "0 / 0")
          (send (ivy-tag-tfield) set-value "")
          (send (ivy-tag-tfield) set-field-background color-white)
          (send (status-bar-dimensions) set-label "0 x 0")
          (send (ivy-canvas) init-auto-scrollbars 100 100 0.0 0.0)
          (collect-garbage 'incremental))]))

(define ivy-menu-bar-filter
  (new menu%
       [parent ivy-menu-bar-file]
       [label "Filter"]
       [help-string "Filter displayed images"]))


(define ivy-menu-bar-filter-tagged
  (new menu-item%
       [parent ivy-menu-bar-filter]
       [label "&Tagged"]
       [shortcut #\T]
       [help-string "Show only tagged images"]
       [callback (λ (i e)
                   (define new-pfs
                     (filter (λ (path) (db-has-key? 'images (path->string path))) (pfs)))
                   (pfs new-pfs)
                   (send (ivy-tag-tfield) set-field-background color-white)
                   (image-path (first new-pfs))
                   (collect-garbage 'incremental)
                   (load-image (image-path))
                   (send (ivy-canvas) refresh-now))]))

(define ivy-menu-bar-filter-untagged
  (new menu-item%
       [parent ivy-menu-bar-filter]
       [label "&Untagged"]
       [shortcut #\U]
       [help-string "Show only untagged images"]
       [callback (λ (i e)
                   (define new-pfs
                     (filter (λ (path) (not (db-has-key? 'images (path->string path)))) (pfs)))
                   (pfs new-pfs)
                   (send (ivy-tag-tfield) set-field-background color-white)
                   (image-path (first new-pfs))
                   (collect-garbage 'incremental)
                   (load-image (image-path))
                   (send (ivy-canvas) refresh-now))]))

(define ivy-menu-bar-search-tag
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Find Images with Tags"]
       [shortcut #\F]
       [help-string "Search for images with specified tags."]
       [callback (λ (i e)
                   (send search-tfield focus)
                   (send (send search-tfield get-editor) select-all)
                   (send search-tag-dialog center 'both)
                   (send search-tag-dialog show #t))]))

(define ivy-menu-bar-sync-db
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "Sync XMP Database"]
       [help-string "Sync the database with the embedded XMP."]
       [callback (λ (i e)
                   (define pairs (table-pairs 'images))
                   (define pairs-len (length pairs))
                   (clean-db!)
                   (display "Checking database: ")
                   (define (worker-loop imgs)
                     (for ([pair (in-list pairs)])
                       (define img-str (path->string (first pair)))
                       (when (embed-support? img-str)
                         (define embed-lst (get-embed-tags img-str))
                         (unless (equal? (sort (second pair) string<?) (sort embed-lst string<?))
                           (printf "Updating tags for ~a\n" img-str)
                           (reconcile-tags! img-str embed-lst))
                         (define ir
                           (cond [(db-has-key? 'ratings img-str)
                                  (number->string (image-rating img-str))]
                                 [else
                                  (set-image-rating! img-str 0)
                                  "0"]))
                         (define xr
                           (let ([embed (get-embed-xmp img-str)])
                             (if (empty? embed)
                                 "0"
                                 (xmp-rating (first embed)))))
                         (unless (string=? ir xr)
                           (printf "Updating rating for ~a\n" img-str)
                           (set-image-rating! img-str (string->number xr)))
                         (sleep 0.1))))
                   (define division (inexact->exact (floor (/ pairs-len 4))))
                   ; if we're dealing with few items, don't make many workers
                   (cond [(<= pairs-len 16)
                          (worker-loop pairs)]
                         [else
                          (define grid (grid-list pairs division))
                          (displayln "Spawning workers...")
                          (for ([g (in-list grid)])
                            (thread (λ () (worker-loop g))))])
                   (displayln "Done."))]))

(define ivy-menu-bar-file-quit
  (if macosx?
      #f
      (new menu-item%
           [parent ivy-menu-bar-file]
           [label "&Quit"]
           [shortcut #\Q]
           [help-string "Quit the program."]
           [callback (λ (i e) (exit:exit))])))

;; Edit menu items ;;

(define ivy-menu-bar-edit-meta-editor
  (new menu-item%
       [parent ivy-menu-bar-edit]
       [label "Metadata &Editor"]
       [help-string "Open the metadata editor."]
       [shortcut #\E]
       [callback (λ (i e) (show-meta-frame))]))

(define ivy-menu-bar-edit-copy-path
  (new menu-item%
       [parent ivy-menu-bar-edit]
       [label "Copy Image Path"]
       [help-string "Copy the current image's path"]
       [callback (λ (i e)
                   (unless (eq? (image-path) +root-path+)
                     (send the-clipboard set-clipboard-string
                           (path->string (image-path))
                           (current-seconds))))]))

;; Navigation menu items ;;

(define ivy-menu-bar-navigation-prev
  (new menu-item%
       [parent ivy-menu-bar-navigation]
       [label "Previous Image"]
       [help-string "Display the Previous Image."]
       [callback (λ (i e) (load-previous-image))]))

(define ivy-menu-bar-navigation-next
  (new menu-item%
       [parent ivy-menu-bar-navigation]
       [label "Next Image"]
       [help-string "Display the Next Image."]
       [callback (λ (i e) (load-next-image))]))

(define ivy-menu-bar-navigation-first
  (new menu-item%
       [parent ivy-menu-bar-navigation]
       [label "First Image"]
       [help-string "Display the First Image."]
       [callback (λ (i e) (load-first-image))]))

(define ivy-menu-bar-navigation-last
  (new menu-item%
       [parent ivy-menu-bar-navigation]
       [label "Last Image"]
       [help-string "Display the Last Image."]
       [callback (λ (i e) (load-last-image))]))

(define ivy-menu-bar-navigation-rand
  (new menu-item%
       [parent ivy-menu-bar-navigation]
       [label "&Random Image"]
       [shortcut #\R]
       [help-string "Display a Random Image."]
       [callback (λ (i e) (load-rand-image))]))

;; View menu items ;;

(define ivy-menu-bar-view-fullscreen
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Fullscreen"]
       [help-string "Enter fullscreen mode."]
       [shortcut (if macosx? #\F #f)]
       [shortcut-prefix (if macosx? '(ctl cmd) (get-default-shortcut-prefix))]
       [callback (λ (i e) (toggle-fullscreen (ivy-canvas) ivy-frame))]))

(define ivy-menu-bar-view-animation
  (new checkable-menu-item%
       [parent ivy-menu-bar-view]
       [label "&Animation"]
       [help-string "Animate image, if possible."]
       [checked (want-animation?)]
       [callback (λ (i e)
                   (want-animation? (send i is-checked?))
                     (when (and (not (equal? (image-path) +root-path+))
                                (or (and (gif? (image-path))
                                         (gif-animated? (image-path)))
                                    (and (flif? (image-path))
                                         (flif-animated? (image-path)))))
                       (load-image (image-path))))]))

(define ivy-menu-bar-view-tag-browser
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Tag &Browser"]
       [shortcut #\B]
       [help-string "Open the Tag Browser."]
       [callback (λ (i e)
                   (show-tag-browser))]))

(define ivy-menu-bar-view-zoom-to
  (new menu%
       [parent ivy-menu-bar-view]
       [label "Zoom To"]
       [help-string "Zoom the image to a specified percentage."]))


(for ([n (in-range 10 110 10)])
  (new menu-item%
       [parent ivy-menu-bar-view-zoom-to]
       [label (format "~a%" n)]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (if (empty? image-lst-master)
                         (load-image (bitmap image-bmp-master) n)
                         (load-image image-lst-master n))))]))

(define ivy-menu-bar-view-rotate-left
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate left"]
       [help-string "Rotate the image left."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (load-image (rotate image-pict (/ pi 2)) 'same)))]))

(define ivy-menu-bar-view-rotate-right
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate right"]
       [help-string "Rotate the image right."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (load-image (rotate image-pict (- (/ pi 2))) 'same)))]))

(define ivy-menu-bar-view-flip-horizontal
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip horizontal"]
       [help-string "Flip the image horizontally."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (define flo
                       (flomap-flip-horizontal (bitmap->flomap (pict->bitmap image-pict))))
                     (collect-garbage 'incremental)
                     (load-image (bitmap (flomap->bitmap flo)) 'same)))]))

(define ivy-menu-bar-view-flip-vertical
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip vertical"]
       [help-string "Flip the image vertically."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (define flo
                       (flomap-flip-vertical (bitmap->flomap (pict->bitmap image-pict))))
                     (collect-garbage 'incremental)
                     (load-image (bitmap (flomap->bitmap flo)) 'same)))]))

(define ivy-menu-bar-view-sort-alpha
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Sort Alphabetically"]
       [help-string "Sort the current collection alphabetically."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     (define new-pfs (sort (pfs) path<?))
                     (pfs new-pfs)
                     (send (status-bar-position)
                           set-label
                           (format "~a / ~a"
                                   (+ (get-index (image-path) (pfs)) 1)
                                   (length (pfs))))))]))

(define ivy-menu-bar-view-sort-rating-high
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Sort by High Rating"]
       [help-string "Sort the current collection by highest Rating."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     ; read the database entries for ratings.
                     ; -1 rating means "don't include"
                     (define ratings
                       (for/fold ([lst empty])
                                 ([img (in-list (map path->string (pfs)))])
                         (define rating (if (db-has-key? 'ratings img)
                                            (image-rating img)
                                            0))
                         (if (= rating -1)
                             lst
                             (append lst (list (cons (string->path img) rating))))))
                     (define new-pfs
                       (sort ratings
                             (λ (a b)
                               (> (cdr a) (cdr b)))))
                     (pfs (map car new-pfs))
                     (send (status-bar-position)
                           set-label
                           (format "~a / ~a"
                                   (+ (get-index (image-path) (pfs)) 1)
                                   (length (pfs))))))]))

(define ivy-menu-bar-view-sort-rating-low
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Sort by Low Rating"]
       [help-string "Sort the current collection by lowest Rating."]
       [callback (λ (i e)
                   (unless (equal? (image-path) +root-path+)
                     ; read the database entries for ratings
                     ; -1 rating means "don't include"
                     (define ratings
                       (for/fold ([lst empty])
                                 ([img (in-list (map path->string (pfs)))])
                         (define rating (if (db-has-key? 'ratings img)
                                            (image-rating img)
                                            0))
                         (if (= rating -1)
                             lst
                             (append lst (list (cons (string->path img) rating))))))
                     (define new-pfs
                       (sort ratings
                             (λ (a b)
                               (< (cdr a) (cdr b)))))
                     (pfs (map car new-pfs))
                     (send (status-bar-position)
                           set-label
                           (format "~a / ~a"
                                   (+ (get-index (image-path) (pfs)) 1)
                                   (length (pfs))))))]))

;; Window menu items ;;

(define ivy-menu-bar-window-minimize
  (when macosx?
    (new menu-item%
         [parent ivy-menu-bar-window]
         [label "&Minimize"]
         [shortcut #\M]
         [help-string "Minimize the Window."]
         [callback (λ (i e) (send ivy-frame iconize #t))])))

; Help window items

(define ivy-menu-bar-help-about
  (new menu-item%
       [parent ivy-menu-bar-help]
       [label "&About"]
       [help-string "Display license information."]
       [callback
        (λ (i e)
          (message-box "Ivy - About"
                       (format "Ivy ~a, the Taggable Image Viewer
Copyright (C) 2016  Lehi Toskin

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>."
                               ivy-version)))]))

(define ivy-menu-bar-help-statistics
  (new menu-item%
       [parent ivy-menu-bar-help]
       [label "&Statistics"]
       [help-string "Display database statistics."]
       [callback (λ (i e)
                   (update-stats)
                   (send stats-frame center 'both)
                   (send stats-frame show #t))]))

(define ivy-menu-bar-help-log
  (new menu-item%
       [parent ivy-menu-bar-help]
       [label "&Error Log"]
       [help-string "Display the error log."]
       [callback (λ (i e)
                   (send log-frame center 'both)
                   (send log-frame show #t))]))

;; main window layout ;;

; left/right, zoom in/out,
; list of tags separated by commas
; e.g. flower,castle,too many cooks,fuzzy wuzzy wuz a bear,etc
(define ivy-toolbar-hpanel (new horizontal-panel%
                                [parent ivy-frame]
                                [stretchable-height #f]))

(define ivy-actions-previous
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (arrow 15 pi))]
       [callback (λ (button event)
                   (load-previous-image))]))

(define ivy-actions-next
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (arrow 15 0))]
       [callback (λ (button event)
                   (load-next-image))]))

; the pict functions are finicky and need to be done juuuust right
; otherwise the circle is cut off on the right side.
(define ivy-actions-zoom-in
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -12 (circle 15) (text "+ ")))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (if (and image-pict
                              (empty? image-lst))
                         (load-image image-pict 'larger)
                         (load-image image-lst 'larger))))]))

(define ivy-actions-zoom-out
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (if (and image-pict
                              (empty? image-lst))
                         (load-image image-pict 'smaller)
                         (load-image image-lst 'smaller))))]))

(define ivy-actions-zoom-normal
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (rectangle 15 15))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (if (empty? image-lst)
                         (load-image image-bmp-master 'none)
                         (load-image (image-path) 'none))))]))

(define ivy-actions-zoom-fit
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -3 (frame (circle 15)) (text " ")))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) +root-path+)
                     (collect-garbage 'incremental)
                     (if (empty? image-lst)
                         (load-image image-bmp-master)
                         (load-image (image-path)))))]))


(ivy-actions-rating
 (new choice%
      [parent ivy-toolbar-hpanel]
      [label ""]
      [choices (for/list ([n (in-range 5 -2 -1)])
                 (string-append (number->string n) " 🌟"))]
      [selection 5] ; "0"
      [stretchable-width #f]
      [stretchable-height #f]
      [callback (λ (choice evt)
                  ; do nothing if we've pressed ctrl+n or if the
                  ; image cannot embed metadata
                  (unless (or (equal? (image-path) +root-path+)
                              (not (embed-support? (image-path))))
                    (define img (image-path))

                    (define (set-xmp:rating!)
                      (define type "xmp:Rating")
                      (define type-sym 'xmp:Rating)
                      (define sel (send choice get-string-selection))
                      (define elems (substring sel 0 (- (string-length sel) 2)))
                      (define attrs "")
                      (define xexpr (if (empty? (unbox image-xmp))
                                        (make-xmp-xexpr empty)
                                        (string->xexpr (first (unbox image-xmp)))))
                      (define xmp (findf-txexpr xexpr (is-tag? type-sym)))
                      (define setted
                        (cond
                          ; if the tag exists as an element, replace it
                          [xmp
                           ((set-xmp-tag type-sym)
                            xexpr
                            (create-dc-meta type elems attrs))]
                          ; otherwise set it as an attr
                          [else
                           ((set-xmp-tag 'rdf:Description)
                            xexpr
                            (create-dc-meta type elems attrs))]))
                      ; set the rating in the database
                      (set-image-rating! (path->string img) (string->number elems))
                      ; set xmp data
                      (set-box! image-xmp (list (xexpr->xmp setted)))
                      (set-embed-xmp! img (first (unbox image-xmp)))
                      ; remove this thread from the tracked threads
                      (hash-remove! xmp-threads img))
                     
                    (cond [(hash-empty? xmp-threads)
                           (hash-set! xmp-threads img
                                      (thread set-xmp:rating!))]
                          [else
                           ; wait for any xmp-threads to finish before continuing
                           (when (hash-has-key? xmp-threads img)
                             (printf "Waiting for thread ~a to finish...~n" img)
                             (sync (hash-ref xmp-threads img)))
                           (hash-set! xmp-threads
                                      img
                                      (thread set-xmp:rating!))])))]))

(define (on-escape-key tfield)
  (unless (equal? (image-path) +root-path+)
    (send tfield set-field-background color-white)
    (define-values (base name-path must-be-dir?) (split-path (image-path)))
    (if (string=? (send tfield get-value) (incoming-tags))
        (send (ivy-canvas) focus)
        (send tfield set-value (incoming-tags)))
    (send ivy-frame set-label (string-truncate (path->string name-path) +label-max+))))

(define ivy-tfield%
  (class text-field%
    (super-new)
    
    (define editor (send this get-editor))
    
    (define/override (on-subwindow-char receiver event)
      (define type (send event get-key-code))
      (case type
        [(escape) (on-escape-key this)]
        [else
         (send editor on-char event)]))))

(ivy-tag-tfield
 (new ivy-tfield%
      [parent ivy-toolbar-hpanel]
      [label "Tags: "]
      [stretchable-height #f]
      [callback
       (λ (tf evt)
         (unless (equal? (image-path) +root-path+)
           (define img (image-path))
           (define img-str (path->string img))
           (define-values (base name-path must-be-dir?) (split-path img))
           (define name-str (path->string name-path))
           (cond [(eq? (send evt get-event-type) 'text-field-enter)
                  (define tags (send tf get-value))
                  (send ivy-frame set-label (string-truncate name-str +label-max+))
                  (when (embed-support? img-str)
                    ; put this into a new thread to speed things up
                    ; wait for any threads on this image to complete
                    (when (hash-has-key? xmp-threads img)
                      (printf "Waiting for thread ~a to finish...~n" img)
                      (sync (hash-ref xmp-threads img)))
                    (hash-set! xmp-threads
                               img
                               (thread (λ ()
                                         (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                         (set-box! image-xmp (get-embed-xmp img-str))
                                         (hash-remove! xmp-threads img)))))
                  (cond [(string-null? tags)
                         ; empty tag string means delete the entry
                         ; no failure if key doesn't exist
                         (db-purge! img-str)]
                        [else
                         (incoming-tags tags)
                         ; turn the string of tag(s) into a list then sort it
                         (define tag-lst (tfield->list tf))
                         ; add/remove tags as necessary
                         (reconcile-tags! img-str tag-lst)])
                  (send tf set-field-background color-spring-green)
                  (when (send browser-frame is-shown?)
                    (show-tag-browser))
                  (send (ivy-canvas) focus)]
                 [else
                  (send ivy-frame set-label
                        (string-truncate (string-append "* " name-str) +label-max+))
                  ; see color-database<%> for more named colors
                  (send tf set-field-background color-gold)])))]))

(define ivy-tag-button
  (new button%
       [parent ivy-toolbar-hpanel]
       [label "Set"]
       [callback
        (λ (button event)
          (unless (equal? (image-path) +root-path+)
            (define img (image-path))
            (define img-str (path->string img))
            (define-values (base name-path must-be-dir?) (split-path (image-path)))
            (send ivy-frame set-label (string-truncate (path->string name-path) +label-max+))
            (define tags (send (ivy-tag-tfield) get-value))
            (send (ivy-tag-tfield) set-field-background color-spring-green)
            (when (embed-support? img-str)
              ; put this into a new thread to speed things up
              ; wait for any threads on this image to complete
              (when (hash-has-key? xmp-threads img)
                (printf "Waiting for thread ~a to finish...~n" img)
                (sync (hash-ref xmp-threads img)))
              (hash-set! xmp-threads
                         img
                         (thread (λ ()
                                   (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                   (set-box! image-xmp (get-embed-xmp img-str))
                                   (hash-remove! xmp-threads img)))))
            ; empty tag string means delete the entry
            (cond [(string-null? tags)
                   ; no failure if key doesn't exist
                   (db-purge! img-str)]
                  [else
                   (incoming-tags tags)
                   ; turn the string of tag(s) into a list then sort it
                   (define tag-lst (tfield->list (ivy-tag-tfield)))
                   ; add/remove tags as necessary
                   (reconcile-tags! img-str tag-lst)])
            (when (send browser-frame is-shown?)
              (show-tag-browser))
            (send (ivy-canvas) focus)))]))

(define (focus-tag-tfield)
  (let ([txt (send (ivy-tag-tfield) get-editor)])
    (send txt set-position (send txt last-position)))
  (send (ivy-tag-tfield) focus))

(define ivy-canvas%
  (class canvas%
    (super-new)
    (init-field paint-callback)
    
    (define mouse-x 0)
    (define mouse-y 0)
    
    (define/public (get-mouse-pos)
      (values mouse-x mouse-y))
    
    (define (do-on-paint)
      (when paint-callback
        (paint-callback this (send this get-dc))))
    
    (define/override (on-paint)
      (do-on-paint))
    
    ; proc: ((is-a?/c canvas%) (is-a?/c dc<%>) . -> . any)
    (define/public (set-on-paint! proc)
      (set! paint-callback proc))
    
    (define/override (on-drop-file pathname)
      ; append the image to the current collection
      (define-values (base name must-be-dir?) (split-path pathname))
      (define directory? (directory-exists? pathname))
      (cond
        ; empty collection
        [(equal? (first (pfs)) +root-path+)
         (cond [directory?
                (define files
                  (for/fold ([lst empty])
                            ([p (in-directory pathname)])
                    (if (supported-file? p)
                        (append lst (list p))
                        lst)))
                (image-dir pathname)
                (pfs files)
                (image-path (first files))
                (load-image (first files))]
               [else
                (image-dir base)
                (pfs (list pathname))
                (image-path pathname)
                (load-image pathname)])]
        ; collection has images; appending to collection
        [else
         (define files
           (if (directory-exists? pathname)
               (for/fold ([lst empty])
                         ([p (in-directory pathname)])
                 (if (supported-file? p)
                     (append lst (list p))
                     lst))
               (list pathname)))
         ; no duplicate paths allowed!
         (pfs (remove-duplicates (append (pfs) files)))
         ; change label because it usually isn't called until
         ; (load-image) is called and we want to see the changes now
         (send (status-bar-position) set-label
               (format "~a / ~a"
                       (+ (get-index (image-path) (pfs)) 1)
                       (length (pfs))))]))
    
    (define/override (on-event evt)
      (define type (send evt get-event-type))
      (case type
        ; track where the mouse is
        [(enter motion)
         (set! mouse-x (send evt get-x))
         (set! mouse-y (send evt get-y))]))
    
    (define/override (on-char key)
      (define type (send key get-key-code))
      (case type
        [(wheel-down)
         ; do nothing if we've pressed ctrl+n
         (unless (equal? (image-path) +root-path+)
           (collect-garbage 'incremental)
           (if (and image-pict
                    (empty? image-lst))
               (load-image image-pict 'wheel-smaller)
               (load-image image-lst 'wheel-smaller)))]
        [(wheel-up)
         ; do nothing if we've pressed ctrl+n
         (unless (equal? (image-path) +root-path+)
           (collect-garbage 'incremental)
           (if (and image-pict
                    (empty? image-lst))
               (load-image image-pict 'wheel-larger)
               (load-image image-lst 'wheel-larger)))]
        ; osx does things a little different
        [(f11) (unless macosx?
                 (toggle-fullscreen this ivy-frame))]
        ; only do something if we're fullscreened,
        ; since the tag bar isn't available in fullscreen anyway
        [(escape) (when (and (send ivy-frame is-fullscreened?) (not macosx?))
                    (toggle-fullscreen this ivy-frame))]
        [(left) (load-previous-image)]
        [(right) (load-next-image)]
        [(home) (load-first-image)]
        [(end) (load-last-image)]
        [(#\,) (focus-tag-tfield)
               (send (send (ivy-tag-tfield) get-editor) insert ", ")]
        [(#\return) (focus-tag-tfield)]))))

(ivy-canvas
 (new ivy-canvas%
      [parent ivy-frame]
      [label "Ivy Image Canvas"]
      [style '(hscroll vscroll)]
      [stretchable-height #t]
      [paint-callback (λ (canvas dc)
                        (send canvas set-canvas-background color-black))]))
(send (ivy-canvas) accept-drop-files #t)

(define status-bar-hpanel
  (new horizontal-panel%
       [parent ivy-frame]
       [stretchable-height #f]))

(define dimensions-hpanel
  (new horizontal-panel%
       [parent status-bar-hpanel]
       [stretchable-height #f]
       [alignment '(left center)]))

(define error-hpanel
  (new horizontal-panel%
       [parent status-bar-hpanel]
       [stretchable-height #f]
       [alignment '(center center)]))

(define position-hpanel
  (new horizontal-panel%
       [parent status-bar-hpanel]
       [stretchable-height #f]
       [alignment '(right center)]))

(status-bar-dimensions
 (new message%
      [parent dimensions-hpanel]
      [label ""]
      [auto-resize #t]))

(status-bar-error
 (new message%
      [parent error-hpanel]
      [label ""]
      [auto-resize #t]))

(status-bar-position
 (new message%
      [parent position-hpanel]
      [label ""]
      [auto-resize #t]))
