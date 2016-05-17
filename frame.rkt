#lang racket/base
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require images/flomap
         pict
         racket/class
         racket/dict
         racket/gui/base
         racket/list
         racket/math
         racket/string
         "base.rkt"
         "db.rkt"
         "files.rkt"
         "search-dialog.rkt")
(provide (all-defined-out))

(define ivy-frame (new frame%
                       [label "Ivy Image Viewer"]
                       [style '(fullscreen-button)]
                       [width 800]
                       [height 600]))

; set the icon for the frame
(unless (macosx?)
  (send ivy-frame set-icon (read-bitmap logo)))

(define ivy-menu-bar (new menu-bar%
                          [parent ivy-frame]))

(define ivy-menu-bar-file (new menu%
                               [parent ivy-menu-bar]
                               [label "&File"]))

(define ivy-menu-bar-navigation (new menu%
                                     [parent ivy-menu-bar]
                                     [label "&Navigation"]))

(define ivy-menu-bar-view (new menu%
                               [parent ivy-menu-bar]
                               [label "&View"]))

(define ivy-menu-bar-window
  (when (macosx?)
    (new menu%
         [parent ivy-menu-bar]
         [label "&Window"])))

;; Fullscreen handling ;;

; awww yeah... so oldskool...
(define (remove-children parent kids)
  (when (> (length kids) 0)
    (send parent delete-child (car kids))
    (remove-children parent (cdr kids))))

; just check out those tail recursions...
(define (add-children parent kids)
  (when (> (length kids) 0)
    (send parent add-child (car kids))
    (add-children parent (cdr kids))))

(define (toggle-fullscreen canvas frame)
  (define was-fullscreen?  (send frame is-fullscreened?))
  (define going-to-be-fullscreen? (not was-fullscreen?))
  ;(eprintf "(toggle-fullscreen ...) going-to-be-fullscreen? == ~v~n" going-to-be-fullscreen?)
  (send frame fullscreen going-to-be-fullscreen?)
  (unless (macosx?)
    (on-fullscreen-event going-to-be-fullscreen?)))

(define (on-fullscreen-event is-fullscreen?)
  ;(eprintf "(on-fucllscreen-event ~v)~n" is-fullscreen?)
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
                                   ;(eprintf "(notify-callback)~n")
                                   (on-fullscreen-event is-fullscreen?)
                                   (was-fullscreen? is-fullscreen?)]))]))
  (let [(default-handler (application-quit-handler))]
    (application-quit-handler
     (λ ()
       (send ivy-fullscreen-poller stop)
       (default-handler)))))

;; File menu items ;;

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
             #f
             (image-dir)
             #f
             #f
             null
             `(("All images"
                ,(string-append
                  "*."
                  (string-join supported-extensions ";*.")))
               ("Any" "*.*"))))
          ; make sure the path is not false
          (when paths
            (define img-path (first paths))
            (cond [(> (length paths) 1) (pfs paths)]
                  [else
                   (define-values (base name dir?) (split-path img-path))
                   (image-dir base)
                   (pfs (path-files))])
            (image-path img-path)
            (load-image img-path)))]))

(define ivy-menu-bar-file-append
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Append images to collection"]
       [shortcut #\O]
       [shortcut-prefix (if (macosx?) '(cmd shift) '(ctl shift))]
       [help-string "Append images to existing collection"]
       [callback
        (λ (i e)
          (define paths
            (get-file-list
             "Select an image or images to view."
             #f
             (image-dir)
             #f
             #f
             null
             `(("All images"
                ,(string-append
                  "*."
                  (string-join supported-extensions ";*.")))
               ("Any" "*.*"))))
          ; the user did not click cancel
          (when paths
            (define path-default? (equal? (first (pfs)) (build-path "/")))
            (cond
              ; empty collection and adding more than 1 image
              [(and path-default? (> (length paths) 1))
               (pfs paths)]
              ; empty collection, adding 1 image
              ; like file-open, but only open the single image
              [path-default?
               (define img-path (first paths))
               (define-values (base name dir?) (split-path img-path))
               (image-dir base)
               (pfs paths)
               (image-path img-path)
               (load-image img-path)]
              ; collection has images; appending to collection
              [else
               (pfs (append (pfs) paths))
               ; change label because it usually isn't called until
               ; (load-image) is called and we want to see the changes now
               (send (status-bar-position) set-label
                     (format "~a / ~a"
                             (+ (get-index (image-path) (pfs)) 1)
                             (length (pfs))))])))]))

(define ivy-menu-bar-file-collection-new
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&New collection"]
       [shortcut #\N]
       [help-string "Empties the current collection"]
       [callback
        (λ (i e)
          (image-dir (find-system-path 'home-dir))
          (pfs empty)
          (image-path (build-path "/"))
          (send (ivy-canvas) set-on-paint!
                (λ ()
                  (send (ivy-canvas) set-canvas-background
                        (make-object color% "black"))))
          (send (ivy-canvas) refresh)
          (send ivy-frame set-label "Ivy Image Viewer")
          (send (status-bar-position) set-label "0 / 0")
          (send (ivy-tag-tfield) set-value "")
          (send (status-bar-dimensions) set-label "0 x 0"))]))

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
           [callback (λ (i e) (disconnect sqlc) (exit))])))

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

(define ivy-menu-bar-view-rotate-left
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate left"]
       [help-string "Rotate the image left."]
       [callback (λ (i e)
                   (load-image (rotate image-pict (/ pi 2)) 'same))]))

(define ivy-menu-bar-view-rotate-right
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Rotate right"]
       [help-string "Rotate the image right."]
       [callback (λ (i e)
                   (load-image (rotate image-pict (- (/ pi 2))) 'same))]))

(define ivy-menu-bar-view-flip-horizontal
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip horizontal"]
       [help-string "Flip the image horizontally."]
       [callback (λ (i e)
                   (define flo
                     (flomap-flip-horizontal (bitmap->flomap (bitmap image-pict))))
                   (load-image (flomap->bitmap flo) 'same))]))

(define ivy-menu-bar-view-flip-vertical
  (new menu-item%
       [parent ivy-menu-bar-view]
       [label "Flip vertical"]
       [help-string "Flip the image vertically."]
       [callback (λ (i e)
                   (define flo
                     (flomap-flip-vertical (bitmap->flomap (bitmap image-pict))))
                   (load-image (flomap->bitmap flo) 'same))]))

;; Window menu items ;;

(define ivy-menu-bar-window-minimize
  (when (macosx?)
    (new menu-item%
         [parent ivy-menu-bar-window]
         [label "&Minimize"]
         [shortcut #\M]
         [help-string "Minimize the Window."]
         [callback (λ (i e) (send ivy-frame iconize #t))])))

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
                   (when image-pict
                     (load-image image-pict 'larger)))]))

(define ivy-actions-zoom-out
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   (when image-pict
                     (load-image image-pict 'smaller)))]))

(define ivy-actions-zoom-normal
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (rectangle 15 15))]
       [callback (λ (button event)
                   (load-image image-bmp-master 'none))]))

(define ivy-actions-zoom-fit
  (new button%
       [parent ivy-toolbar-hpanel]
       [label (pict->bitmap (hc-append -3 (frame (circle 15)) (text " ")))]
       [callback (λ (button event)
                   (load-image image-bmp-master))]))

(define (on-escape-key tfield)
  (define current-tags (send tfield get-value))
  (send tfield set-field-background (make-object color% "white"))
  (cond [(string=? current-tags (incoming-tags))
         (send (ivy-canvas) focus)]
        [else (send tfield set-value (incoming-tags))
              (define-values (base name-sym must-be-dir?) (split-path (image-path)))
              (send ivy-frame set-label (path->string name-sym))]))

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
         (define img-str (path->string (image-path)))
         (unless (string=? img-str "/")
           (define-values (base name-path must-be-dir?) (split-path (image-path)))
           (define name-str (path->string name-path))
           (cond [(eq? (send evt get-event-type) 'text-field-enter)
                  (define tags (send tf get-value))
                  (send ivy-frame set-label name-str)
                  (cond [(string=? tags "")
                         ; empty tag string means delete the entry
                         ; no failure if key doesn't exist
                         (db-remove! img-str)]
                        [else
                         ; turn the string of tag(s) into a list then sort it
                         (define tag-lst (tfield->list tf))
                         ; set and save the dictionary
                         (db-set! img-str tag-lst)])
                  (send tf set-field-background (make-object color% "spring green"))
                  (send (ivy-canvas) focus)]
                 [else
                  (send ivy-frame set-label (string-append "* " name-str))
                  ; see color-database<%> for more named colors
                  (send tf set-field-background (make-object color% "gold"))])))]))

(define ivy-tag-button
  (new button%
       [parent ivy-toolbar-hpanel]
       [label "Set"]
       [callback
        (λ (button event)
          (define img-str (path->string (image-path)))
          (unless (string=? img-str "/")
            (define-values (base name-path must-be-dir?) (split-path (image-path)))
            (send ivy-frame set-label (path->string name-path))
            (define tags (send (ivy-tag-tfield) get-value))
            (send (ivy-tag-tfield) set-field-background
                  (make-object color% "spring green"))
            ; empty tag string means delete the entry
            (cond [(string=? tags "")
                   ; no failure if key doesn't exist
                   (db-remove! img-str)]
                  [else
                   ; turn the string of tag(s) into a list then sort it
                   (define tag-lst (sort (string-split tags ", ") string<?))
                   ; set and save the dictionary
                   (db-set! img-str tag-lst)])
            (send (ivy-canvas) focus)))]))

(define (focus-tag-tfield)
  (send (ivy-tag-tfield) focus))

(define ivy-canvas%
  (class canvas%
    (super-new)
    (init-field paint-callback)
    
    (define (do-on-paint)
      (when paint-callback
        (paint-callback this (send this get-dc))))
    
    (define/override (on-paint)
      (do-on-paint))
    
    (define/public (set-on-paint! thunk)
      (set! do-on-paint thunk))
    
    (define/override (on-char key)
      (define type (send key get-key-code))
      (case type
        [(wheel-down)
         (when image-pict
           (load-image image-pict 'smaller))]
        [(wheel-up)
         (when image-pict
           (load-image image-pict 'larger))]
        [(f11) (cond [(not (macosx?))
                      (toggle-fullscreen this ivy-frame)])]
        [(left) (load-previous-image)]
        [(right) (load-next-image)]
        [(home) (load-first-image)]
        [(end) (load-last-image)]
        [(#\return) (focus-tag-tfield)]))))

(ivy-canvas
 (new ivy-canvas%
      [parent ivy-frame]
      [label "Ivy Image Canvas"]
      [style '(hscroll vscroll)]
      [stretchable-height #t]
      [paint-callback (λ (canvas dc)
                        (send canvas set-canvas-background
                              (make-object color% "black")))]))

(define status-bar-hpanel
  (new horizontal-panel%
       [parent ivy-frame]
       [stretchable-height #f]))

(define dimensions-hpanel
  (new horizontal-panel%
       [parent status-bar-hpanel]
       [stretchable-height #f]
       [alignment '(left center)]))

(define position-hpanel
  (new horizontal-panel%
       [parent status-bar-hpanel]
       [stretchable-height #f]
       [alignment '(right center)]))

(status-bar-dimensions
 (new message%
      [parent dimensions-hpanel]
      [label (format "~a x ~a pixels"
                     (send image-bmp-master get-width)
                     (send image-bmp-master get-height))]
      [auto-resize #t]))

(status-bar-position
 (new message%
      [parent position-hpanel]
      [label "0 / 0"]
      [auto-resize #t]))
