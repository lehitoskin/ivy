#lang racket/base
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require images/flomap
         pict
         racket/bool
         racket/class
         racket/gui/base
         racket/list
         racket/math
         racket/string
         "base.rkt"
         "db.rkt"
         "db-statistics.rkt"
         "embed.rkt"
         "error-log.rkt"
         "files.rkt"
         "meta-editor.rkt"
         "search-dialog.rkt"
         "tag-browser.rkt")
(provide (all-defined-out))

(define ivy-frame
  (new frame%
       [label "Ivy Image Viewer"]
       [style '(fullscreen-button)]
       [width 800]
       [height 600]))

; set the icon for the frame
(unless (macosx?)
  (send ivy-frame set-icon (read-bitmap logo)))

(define ivy-menu-bar
  (new menu-bar%
       [parent ivy-frame]))

(define ivy-menu-bar-file
  (new menu%
       [parent ivy-menu-bar]
       [label "&File"]))

(define ivy-menu-bar-navigation
  (new menu%
       [parent ivy-menu-bar]
       [label "&Navigation"]))

(define ivy-menu-bar-view
  (new menu%
       [parent ivy-menu-bar]
       [label "&View"]))

(define ivy-menu-bar-window
  (when (macosx?)
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
  (unless (macosx?)
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
(when (macosx?)
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
                  (string-join supported-extensions ";*")))
               ("Any" "*.*"))))
          ; make sure the path is not false
          (when paths
            (define img-path (first paths))
            (cond [(> (length paths) 1) (pfs paths)]
                  [else
                   (define-values (base name dir?) (split-path img-path))
                   (image-dir base)
                   (pfs (path-files))])
            (send (ivy-tag-tfield) set-field-background color-white)
            (image-path img-path)
            (collect-garbage 'incremental)
            (load-image img-path)))]))

(define ivy-menu-bar-file-append
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "Append images to c&ollection"]
       [shortcut #\O]
       [shortcut-prefix (if (macosx?) '(cmd shift) '(ctl shift))]
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
                  (string-join supported-extensions ";*")))
               ("Any" "*.*"))))
          ; the user did not click cancel
          (when paths
            (cond
              ; empty collection, adding images and load the first in the list
              [(equal? (first (pfs)) root-path)
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

; reset the GUI to defaults
(define ivy-menu-bar-file-collection-new
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&New collection"]
       [shortcut #\N]
       [help-string "Empties the current collection"]
       [callback
        (λ (i e)
          (unless (or (false? (gif-thread)) (thread-dead? (gif-thread)))
            (kill-thread (gif-thread)))
          (image-dir (find-system-path 'home-dir))
          (pfs (list root-path))
          (image-path root-path)
          (send (ivy-canvas) set-on-paint!
                (λ (canvas dc)
                  (send canvas set-canvas-background color-black)))
          (send (ivy-canvas) refresh)
          (send ivy-frame set-label "Ivy Image Viewer")
          (send (status-bar-position) set-label "0 / 0")
          (send (ivy-tag-tfield) set-value "")
          (send (ivy-tag-tfield) set-field-background color-white)
          (send (status-bar-dimensions) set-label "0 x 0")
          (send (ivy-canvas) init-auto-scrollbars 100 100 0.0 0.0)
          (collect-garbage 'incremental))]))

(define ivy-menu-bar-search-tag
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Find Images with Tags"]
       [shortcut #\F]
       [help-string "Search for images with specified tags."]
       [callback (λ (i e)
                   (send search-tfield focus)
                   (send (send search-tfield get-editor) select-all)
                   (send search-tag-dialog show #t))]))

(define ivy-menu-bar-file-quit
  (if (macosx?)
      #f
      (new menu-item%
           [parent ivy-menu-bar-file]
           [label "&Quit"]
           [shortcut #\Q]
           [help-string "Quit the program."]
           [callback (λ (i e)
                       ; kill the gif thread, if applicable
                       (unless (or (false? (gif-thread)) (thread-dead? (gif-thread)))
                         (kill-thread (gif-thread)))
                       ; wait for any xmp threads to finish before exiting
                       (unless (zero? (hash-count (xmp-threads)))
                         (for ([pair (in-hash-pairs (xmp-threads))])
                           (let loop ()
                             (unless (thread-dead? (cdr pair))
                               (printf "Waiting for thread ~a to finish...\n" (car pair))
                               (sleep 1/4)
                               (loop)))))
                       (disconnect sqlc)
                       (exit))])))

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
       [shortcut (if (macosx?) #\F #f)]
       [shortcut-prefix (if (macosx?) '(ctl cmd) (get-default-shortcut-prefix))]
       [callback (λ (i e) (toggle-fullscreen (ivy-canvas) ivy-frame))]))

(define ivy-menu-bar-view-gif-animation
  (new checkable-menu-item%
       [parent ivy-menu-bar-view]
       [label "&GIF Animation"]
       [help-string "Animate GIFs, if possible."]
       [callback (λ (i e)
                   (want-animation? (send i is-checked?))
                   (when (and (not (equal? (image-path) root-path))
                              (gif? (image-path))
                              (gif-animated? (image-path)))
                     (collect-garbage 'incremental)
                     (load-image (image-path))))]))

(define ivy-menu-bar-view-tag-browser
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Tag &Browser"]
       [shortcut #\B]
       [help-string "Open the Tag Browser."]
       [callback (λ (i e)
                   (show-tag-browser))]))

(define ivy-menu-bar-view-meta-editor
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Metadata Editor"]
       [help-string "Open the metadata editor."]
       [callback (λ (i e)
                   (show-meta-frame))]))

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
                   (collect-garbage 'incremental)
                   (if (empty? master-gif)
                       (load-image (bitmap image-bmp-master) n)
                       (load-image master-gif n)))]))

(define ivy-menu-bar-view-rotate-left
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate left"]
       [help-string "Rotate the image left."]
       [callback (λ (i e)
                   (collect-garbage 'incremental)
                   (load-image (rotate image-pict (/ pi 2)) 'same))]))

(define ivy-menu-bar-view-rotate-right
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate right"]
       [help-string "Rotate the image right."]
       [callback (λ (i e)
                   (collect-garbage 'incremental)
                   (load-image (rotate image-pict (- (/ pi 2))) 'same))]))

(define ivy-menu-bar-view-flip-horizontal
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip horizontal"]
       [help-string "Flip the image horizontally."]
       [callback (λ (i e)
                   (define flo
                     (flomap-flip-horizontal (bitmap->flomap (pict->bitmap image-pict))))
                   (collect-garbage 'incremental)
                   (load-image (bitmap (flomap->bitmap flo)) 'same))]))

(define ivy-menu-bar-view-flip-vertical
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip vertical"]
       [help-string "Flip the image vertically."]
       [callback (λ (i e)
                   (define flo
                     (flomap-flip-vertical (bitmap->flomap (pict->bitmap image-pict))))
                   (collect-garbage 'incremental)
                   (load-image (bitmap (flomap->bitmap flo)) 'same))]))

;; Window menu items ;;

(define ivy-menu-bar-window-minimize
  (when (macosx?)
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
                       "Ivy, the Taggable Image Viewer
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
along with this program.  If not, see <http://www.gnu.org/licenses/>."))]))

(define ivy-menu-bar-help-statistics
  (new menu-item%
       [parent ivy-menu-bar-help]
       [label "&Statistics"]
       [help-string "Display database statistics."]
       [callback (λ (i e)
                   (update-stats)
                   (send stats-frame show #t))]))

(define ivy-menu-bar-help-log
  (new menu-item%
       [parent ivy-menu-bar-help]
       [label "&Error Log"]
       [help-string "Display the error log."]
       [callback (λ (i e)
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
                   (unless (equal? (image-path) root-path)
                     (collect-garbage 'incremental)
                     (if (and image-pict
                              (empty? gif-lst))
                         (load-image image-pict 'larger)
                         (load-image gif-lst 'larger))))]))

(define ivy-actions-zoom-out
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) root-path)
                     (collect-garbage 'incremental)
                     (if (and image-pict
                              (empty? gif-lst))
                         (load-image image-pict 'smaller)
                         (load-image gif-lst 'smaller))))]))

(define ivy-actions-zoom-normal
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (rectangle 15 15))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) root-path)
                     (collect-garbage 'incremental)
                     (if (empty? gif-lst)
                         (load-image image-bmp-master 'none)
                         (load-image (image-path) 'none))))]))

(define ivy-actions-zoom-fit
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -3 (frame (circle 15)) (text " ")))]
       [callback (λ (button event)
                   ; do nothing if we've pressed ctrl+n
                   (unless (equal? (image-path) root-path)
                     (collect-garbage 'incremental)
                     (if (empty? gif-lst)
                         (load-image image-bmp-master)
                         (load-image (image-path)))))]))

(define (on-escape-key tfield)
  (unless (equal? (image-path) root-path)
    (send tfield set-field-background color-white)
    (define-values (base name-path must-be-dir?) (split-path (image-path)))
    (if (string=? (send tfield get-value) (incoming-tags))
        (send (ivy-canvas) focus)
        (send tfield set-value (incoming-tags)))
    (send ivy-frame set-label (path->string name-path))))

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
         (unless (equal? (image-path) root-path)
           (define img-str (path->string (image-path)))
           (define-values (base name-path must-be-dir?) (split-path (image-path)))
           (define name-str (path->string name-path))
           (cond [(eq? (send evt get-event-type) 'text-field-enter)
                  (define tags (send tf get-value))
                  (send ivy-frame set-label name-str)
                  (when (embed-support? img-str)
                    ; put this into a new thread to speed things up
                    (define time (current-seconds))
                    (if (zero? (hash-count (xmp-threads)))
                        (xmp-threads
                         (hash time
                               (thread (λ ()
                                         (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                         (image-xmp (get-embed-xmp img-str))
                                         (xmp-threads (hash-remove (xmp-threads) time))))))
                        (xmp-threads
                         (hash-set (xmp-threads)
                                   time
                                   (thread (λ ()
                                             (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                             (image-xmp (get-embed-xmp img-str))
                                             (xmp-threads (hash-remove (xmp-threads) time))))))))
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
                  (send ivy-frame set-label (string-append "* " name-str))
                  ; see color-database<%> for more named colors
                  (send tf set-field-background color-gold)])))]))

(define ivy-tag-button
  (new button%
       [parent ivy-toolbar-hpanel]
       [label "Set"]
       [callback
        (λ (button event)
          (unless (equal? (image-path) root-path)
            (define img-str (path->string (image-path)))
            (define-values (base name-path must-be-dir?) (split-path (image-path)))
            (send ivy-frame set-label (path->string name-path))
            (define tags (send (ivy-tag-tfield) get-value))
            (send (ivy-tag-tfield) set-field-background color-spring-green)
            (when (embed-support? img-str)
              ; put this into a new thread to speed things up
              (define time (current-seconds))
              (if (zero? (hash-count (xmp-threads)))
                  (xmp-threads
                   (hash time
                         (thread (λ ()
                                   (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                   (image-xmp (get-embed-xmp img-str))
                                   (xmp-threads (hash-remove (xmp-threads) time))))))
                  (xmp-threads
                   (hash-set (xmp-threads)
                             time
                             (thread (λ ()
                                       (set-embed-tags! img-str (tfield->list (ivy-tag-tfield)))
                                       (image-xmp (get-embed-xmp img-str))
                                       (xmp-threads (hash-remove (xmp-threads) time))))))))
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
      (cond
        ; empty collection, adding 1 image
        ; like file-open, but only open the single image
        [(equal? (first (pfs)) root-path)
         (define-values (base name dir?) (split-path pathname))
         (image-dir base)
         (pfs (list pathname))
         (image-path pathname)
         (collect-garbage 'incremental)
         (load-image pathname)]
        ; collection has images; appending to collection
        [else
         ; no duplicate paths allowed!
         (pfs (remove-duplicates (append (pfs) (list pathname))))
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
         (unless (equal? (image-path) root-path)
           (collect-garbage 'incremental)
           (if (and image-pict
                    (empty? gif-lst))
               (load-image image-pict 'wheel-smaller)
               (load-image gif-lst 'wheel-smaller)))]
        [(wheel-up)
         ; do nothing if we've pressed ctrl+n
         (unless (equal? (image-path) root-path)
           (collect-garbage 'incremental)
           (if (and image-pict
                    (empty? gif-lst))
               (load-image image-pict 'wheel-larger)
               (load-image gif-lst 'wheel-larger)))]
        ; osx does things a little different
        [(f11) (unless (macosx?)
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
