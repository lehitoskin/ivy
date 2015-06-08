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

(define ivy-menu-bar (new menu-bar%
                          [parent ivy-frame]))

(define ivy-menu-bar-file (new menu%
                               [parent ivy-menu-bar]
                               [label "&File"]))

(define ivy-menu-bar-file-open
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Open"]
       [shortcut #\O]
       [help-string "Open a file to view."]
       [callback (λ (i e)
                   (define path (get-file "Select an image to view."
                                          #f
                                          image-dir))
                   ; make sure the path is not false
                   (when path
                     (load-image path)
                     (pfs (path-files))
                     (define index (get-index (symbol->path image-path) (pfs)))
                     (send status-bar-position set-label
                           (format "~a / ~a" (+ index 1) (length (pfs))))))]))

(define ivy-menu-bar-search-tag
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Search Tags"]
       [help-string "Search tags for an image."]
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
                          [label "Search for tags: "]))
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
                             (send search-tag-dialog show #f)
                             (define tags
                               (sort (string-split
                                      (send search-tfield get-value) ",") string<?))
                             (define search-type
                               (string->symbol
                                (send type-rbox get-item-label
                                      (send type-rbox get-selection))))
                             (display-tags search-type tags))]))
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

(define ivy-action-previous
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (arrow 15 pi))]
       [callback (λ (button event)
                   (unless (eq? image-path '/)
                     (define index (get-index (symbol->path image-path) (pfs)))
                     (when (and index (> (length (pfs)) 1))
                       (cond [(zero? index)
                              (define img (last (pfs))) ; this is a path
                              (define cur-pos (get-index img (pfs)))
                              (load-image img)
                              (send status-bar-position set-label
                                    (format "~a / ~a" (+ cur-pos 1) (length (pfs))))]
                             [else
                              (define img (list-ref (pfs) (- index 1)))
                              (define cur-pos (get-index img (pfs)))
                              (load-image img)
                              (send status-bar-position set-label
                                    (format "~a / ~a" (+ cur-pos 1) (length (pfs))))]))))]))

(define ivy-actions-next
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (arrow 15 0))]
       [callback (λ (button event)
                   (unless (eq? image-path '/)
                     (define prev-index (get-index (symbol->path image-path) (pfs)))
                     (when (and prev-index (> (length (pfs)) 1))
                       (cond [(= prev-index (- (length (pfs)) 1))
                              (define img (first (pfs))) ; this is a path
                              (define cur-pos (get-index img (pfs)))
                              (load-image img)
                              (send status-bar-position set-label
                                    (format "~a / ~a" (+ cur-pos 1) (length (pfs))))]
                             [else
                              (define img (list-ref (pfs) (+ prev-index 1)))
                              (define cur-pos (get-index img (pfs)))
                              (load-image img)
                              (send status-bar-position set-label
                                    (format "~a / ~a" (+ cur-pos 1) (length (pfs))))]))))]))

; the pict functions are finicky and need to be done juuuust right
; otherwise the circle is cut off on the right side.
(define ivy-actions-zoom-in
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -12 (circle 15) (text "+ ")))]
       [callback (λ (button event)
                   (when image-pict
                     (load-image image-pict 'larger)
                     #|(define index (get-index (symbol->path image-path) (pfs)))
                     (send status-bar-position set-label
                           (format "~a / ~a" (+ index 1) (length (pfs))))|#))]))

(define ivy-actions-zoom-out
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   (when image-pict
                     (load-image image-pict 'smaller)
                     #|(define index (get-index (symbol->path image-path) (pfs)))
                     (send status-bar-position set-label
                           (format "~a / ~a" (+ index 1) (length (pfs))))|#))]))

(define ivy-actions-zoom-normal
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (rectangle 15 15))]
       [callback (λ (button event)
                   (load-image image-bmp-master 'none)
                   #|(define index (get-index (symbol->path image-path) (pfs)))
                   (send status-bar-position set-label
                         (format "~a / ~a" (+ index 1) (length (pfs))))|#)]))

(define ivy-actions-zoom-fit
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -3 (frame (circle 15)) (text " ")))]
       [callback (λ (button event)
                   (load-image image-bmp-master)
                   #|(define index (get-index (symbol->path image-path) (pfs)))
                   (send status-bar-position set-label
                         (format "~a / ~a" (+ index 1) (length (pfs))))|#)]))

; list of tags separated by commas
; e.g. flower,castle,too many cooks,fuzzy wuzzy wuz a bear,etc
(define ivy-tag-hpanel (new horizontal-panel%
                            [parent ivy-frame]
                            [stretchable-height #f]))

(ivy-tag-tfield
 (new text-field%
      [parent ivy-tag-hpanel]
      [label "Edit tag(s) : "]
      [callback (λ (tf evt)
                  (cond [(eq? (send evt get-event-type) 'text-field-enter)
                         (define tags (send tf get-value))
                         (send tf set-label "Edit tag(s) : ")
                         ; empty tag string means delete the entry
                         (cond [(string=? tags "")
                                ; no failure if key doesn't exist
                                (dict-remove! master image-path)]
                               [(not (eq? image-path '/))
                                ; turn the string of tag(s) into a list then sort it
                                (define tag-lst (sort (string-split tags ",") string<?))
                                ; set and save the dictionary
                                (dict-set! master image-path tag-lst)
                                (save-dict! master)])]
                        [else (send tf set-label "Edit tag(s)*: ")]))]))

(define ivy-tag-button
  (new button%
       [parent ivy-tag-hpanel]
       [label "Set"]
       [callback (λ (button event)
                   (define tags (send (ivy-tag-tfield) get-value))
                   (send (ivy-tag-tfield) set-label "Edit tag(s) : ")
                   ; empty tag string means delete the entry
                   (cond [(string=? tags "")
                          ; no failure if key doesn't exist
                          (dict-remove! master image-path)]
                         [(not (eq? image-path '/))
                          ; turn the string of tag(s) into a list then sort it
                          (define tag-lst (sort (string-split tags ",") string<?))
                          ; set and save the dictionary
                          (dict-set! master image-path tag-lst)
                          (save-dict! master)]))]))

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
           (load-image image-pict 'smaller)
           #|(define index (get-index (symbol->path image-path) (pfs)))
             (send status-bar-position set-label
                   (format "~a / ~a" (+ index 1) (length (pfs))))|#)]
        [(wheel-up)
         (when image-pict
           (load-image image-pict 'larger)
           #|(define index (get-index (symbol->path image-path) (pfs)))
             (send status-bar-position set-label
                   (format "~a / ~a" (+ index 1) (length (pfs))))|#)]))))

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

(define status-bar-position
  (new message%
       [parent position-hpanel]
       [label "0 / 0"]
       [auto-resize #t]))
