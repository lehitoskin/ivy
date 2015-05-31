#lang racket/gui
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require "base.rkt")
(provide (all-defined-out))

(define (path->symbol p)
  (string->symbol (path->string p)))

(define image-path (make-parameter '/))
(define image-bmp (make-parameter (make-bitmap 50 50)))
(define previous-path (find-system-path 'home-dir))

(define (load-image path)
  (define-values (base name must-be-dir?) (split-path path))
  (set! previous-path base)
  (image-path (path->symbol path))
  ; make sure the bitmap loaded correctly
  (define load-success (send (image-bmp) load-file path))
  (cond [load-success
         ; if we've set tags for this file before...
         (cond [(hash-has-key? master (image-path))
                (define tag
                  (string-join (hash-ref master (image-path)) ","))
                ; ...put them in the tfield
                (send ivy-tag-tfield set-value tag)]
               ; ...otherwise clear the tfield
               [else (send ivy-tag-tfield set-value "")])
         
         (define width (send (image-bmp) get-width))
         (define height (send (image-bmp) get-height))
         (send ivy-canvas set-on-paint!
               (λ ()
                 (send ivy-canvas set-canvas-background
                       (make-object color% "black"))
                 (send (send ivy-canvas get-dc) draw-bitmap
                       (image-bmp) 0 0)))
         (send ivy-canvas init-auto-scrollbars width height 0.0 0.0)
         (send ivy-canvas refresh)]
        [else (printf "Error loading file ~a~n" path)]))

(define ivy-frame (new frame%
                       [label "Ivy Image Viewer"]
                       [width 800]
                       [height 600]))

(define ivy-menu-bar (new menu-bar%
                          [parent ivy-frame]))

(define ivy-menu-bar-file (new menu%
                               [parent ivy-menu-bar]
                               [label "&File"]))

#|
; turn the bitmap into a pict
(define icon-pict (bitmap icon-bitmap))
; scale the pict to 40x40
(define icon-pict-small (scale-to-fit icon-pict 40 40))
; set avatar parameter to our scaled icon
(my-avatar (pict->bitmap icon-pict-small))
|#

(define ivy-menu-bar-file-open
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Open"]
       [shortcut #\O]
       [help-string "Open a file to view."]
       [callback (λ (i e)
                   (define path (get-file "Select an image to view."
                                          #f
                                          previous-path))
                   ; make sure the path is a real path
                   (when path (load-image path)))]))

(define ivy-menu-bar-file-quit
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Quit"]
       [shortcut #\Q]
       [help-string "Quit the program."]
       [callback (λ (i e) (exit))]))

(define ivy-tag-hpanel (new horizontal-panel% [parent ivy-frame]))

; list of tags separated by commas
; e.g. flower,castle,too many cooks,fuzzy wuzzy wuz a bear,etc
(define ivy-tag-tfield
  (new text-field%
       [parent ivy-tag-hpanel]
       [label "Edit tag(s): "]))

(define ivy-tag-button
  (new button%
       [parent ivy-tag-hpanel]
       [label "Set"]
       [callback (λ (button event)
                   (define tag (send ivy-tag-tfield get-value))
                   ; empty tag string means delete the entry
                   (cond [(string=? tag "")
                          ; no failure if key doesn't exist
                          (dict-remove! master (image-path))]
                         [(not (eq? (image-path) '/))
                          ; turn the string of tag(s) into a list then sort it
                          (define tag-lst (sort (string-split tag ",") string<?))
                          ; set and save the dictionary
                          (dict-set! master (image-path) tag-lst)
                          (save-dict master)]))]))

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
      (set! do-on-paint thunk))))

(define ivy-canvas
  (new ivy-canvas%
       [parent ivy-frame]
       [label "Ivy Image Canvas"]
       [style '(hscroll vscroll)]
       [min-height 550]
       [paint-callback (λ (canvas dc)
                         (send canvas set-canvas-background
                               (make-object color% "black")))]))
