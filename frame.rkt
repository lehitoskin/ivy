#lang racket/gui
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require pict
         "base.rkt"
         "search-results.rkt")
(provide (all-defined-out))

(define ivy-frame (new frame%
                       [label "Ivy Image Viewer"]
                       [width 800]
                       [height 600]))

; set the icon for the frame
(send ivy-frame set-icon (read-bitmap logo))

(define ivy-menu-bar (new menu-bar%
                          [parent ivy-frame]))

(define ivy-menu-bar-file (new menu%
                               [parent ivy-menu-bar]
                               [label "&File"]))

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
          (define paths (get-file-list
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
       [shortcut-prefix '(ctl shift)]
       [help-string "Append images to existing collection"]
       [callback
        (λ (i e)
          (define paths (get-file-list
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
       [help-string "Search for images with tags."]
       [callback (λ (i e)
                   (define search-tag-dialog
                     (new dialog%
                          [label "Ivy - Search Tags"]
                          [width 400]
                          [height 100]
                          [style '(close-button)]))
                   (define search-tfield
                     (new text-field%
                          [parent search-tag-dialog]
                          [label "Search for tags: "]
                          [callback
                           (λ (tf evt)
                             (when (and
                                    (eq? (send evt get-event-type) 'text-field-enter)
                                    (not (string=? (send tf get-value) "")))
                               (send search-tag-dialog show #f)
                               (define tags
                                 (sort (string-split
                                        (send search-tfield get-value) ", ")
                                       string<?))
                               (define search-type
                                 (string->symbol
                                  (send type-rbox get-item-label
                                        (send type-rbox get-selection))))
                               (display-tags search-type tags)))]))
                   (define type-rbox
                     (new radio-box%
                          [parent search-tag-dialog]
                          [label "Search type"]
                          [choices '("or" "and")]))
                   (define button-hpanel
                     (new horizontal-panel%
                          [parent search-tag-dialog]
                          [alignment '(right center)]
                          [stretchable-height #f]))
                   (define cancel-button
                     (new button%
                          [parent button-hpanel]
                          [label "&Cancel"]
                          [callback (λ (button event)
                                      (send search-tag-dialog show #f))]))
                   (define ok-button
                     (new button%
                          [parent button-hpanel]
                          [label "&Ok"]
                          [callback
                           (λ (button event)
                             (unless (string=? (send search-tfield get-value) "")
                               (send search-tag-dialog show #f)
                               (define tags
                                 (sort (string-split
                                        (send search-tfield get-value) ", ")
                                       string<?))
                               (define search-type
                                 (string->symbol
                                  (send type-rbox get-item-label
                                        (send type-rbox get-selection))))
                               (display-tags search-type tags)))]))
                   (define piggyback (new editor-canvas%
                                          [parent search-tag-dialog]
                                          [editor (send search-tfield get-editor)]))
                   (send search-tfield focus)
                   (send search-tag-dialog delete-child piggyback)
                   (send search-tag-dialog show #t))]))

(define ivy-menu-bar-file-quit
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Quit"]
       [shortcut #\Q]
       [help-string "Quit the program."]
       [callback (λ (i e) (exit))]))

; left/right, zoom in/out
(define actions-hpanel (new horizontal-panel%
                            [parent ivy-frame]
                            [alignment '(center center)]
                            [stretchable-height #f]))

(define ivy-actions-previous
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (arrow 15 pi))]
       [callback (λ (button event)
                   (load-previous-image))]))

(define ivy-actions-next
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (arrow 15 0))]
       [callback (λ (button event)
                   (load-next-image))]))

; the pict functions are finicky and need to be done juuuust right
; otherwise the circle is cut off on the right side.
(define ivy-actions-zoom-in
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -12 (circle 15) (text "+ ")))]
       [callback (λ (button event)
                   (when image-pict
                     (load-image image-pict 'larger)))]))

(define ivy-actions-zoom-out
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   (when image-pict
                     (load-image image-pict 'smaller)))]))

(define ivy-actions-zoom-normal
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (rectangle 15 15))]
       [callback (λ (button event)
                   (load-image image-bmp-master 'none))]))

(define ivy-actions-zoom-fit
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -3 (frame (circle 15)) (text " ")))]
       [callback (λ (button event)
                   (load-image image-bmp-master))]))

; list of tags separated by commas
; e.g. flower,castle,too many cooks,fuzzy wuzzy wuz a bear,etc
(define ivy-tag-hpanel (new horizontal-panel%
                            [parent ivy-frame]
                            [stretchable-height #f]))

(ivy-tag-tfield
 (new text-field%
      [parent ivy-tag-hpanel]
      [label "Edit tag(s) : "]
      [callback
       (λ (tf evt)
         (cond [(eq? (send evt get-event-type) 'text-field-enter)
                (define tags (send tf get-value))
                (send tf set-label "Edit tag(s) : ")
                (define img-sym (path->symbol (image-path)))
                (cond [(string=? tags "")
                       ; empty tag string means delete the entry
                       ; no failure if key doesn't exist
                       (dict-remove! master img-sym)
                       (save-dict! master)]
                      [(not (eq? img-sym '/))
                       ; turn the string of tag(s) into a list then sort it
                       (define tag-lst (sort (string-split tags ", ") string<?))
                       ; set and save the dictionary
                       (dict-set! master img-sym tag-lst)
                       (save-dict! master)])
                (send tf set-field-background (make-object color% "spring green"))
                (send (ivy-canvas) focus)]
               [else
                (send tf set-label "Edit tag(s)*: ")
                ; see color-database<%> for more named colors
                (send tf set-field-background (make-object color% "gold"))]))]))

(define piggyback
  (new editor-canvas%
       [parent ivy-tag-hpanel]
       [editor (send (ivy-tag-tfield) get-editor)]))

(send ivy-tag-hpanel delete-child piggyback)

(define ivy-tag-button
  (new button%
       [parent ivy-tag-hpanel]
       [label "Set"]
       [callback
        (λ (button event)
          (define tags (send (ivy-tag-tfield) get-value))
          (send (ivy-tag-tfield) set-label "Edit tag(s) : ")
          (send (ivy-tag-tfield) set-field-background
                (make-object color% "spring green"))
          (define img-sym (path->symbol (image-path)))
          ; empty tag string means delete the entry
          (cond [(string=? tags "")
                 ; no failure if key doesn't exist
                 (dict-remove! master img-sym)
                 (save-dict! master)]
                [(not (eq? img-sym '/))
                 ; turn the string of tag(s) into a list then sort it
                 (define tag-lst (sort (string-split tags ", ") string<?))
                 ; set and save the dictionary
                 (dict-set! master img-sym tag-lst)
                 (save-dict! master)])
          (send (ivy-canvas) focus))]))

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
        [(left) (load-previous-image)]
        [(right) (load-next-image)]
        [(home) (load-first-image)]
        [(end) (load-last-image)]))))

(ivy-canvas
 (new ivy-canvas%
      [parent ivy-frame]
      [label "Ivy Image Canvas"]
      [style '(hscroll vscroll)]
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
