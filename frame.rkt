#lang racket/gui
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require "base.rkt" pict)
(provide (all-defined-out))

; path of the currently displayed image
(define image-path '/)
; bitmap of the currently displayed image
(define image-bmp (make-bitmap 50 50))
; directory containing the currently displayed image
(define image-dir (find-system-path 'home-dir))
; all files contained within image-dir
(define (path-files)
  (directory-list image-dir #:build? #t))

; get index of an item in the list
; numbering starts from 0
(define (get-index item lst)
  (define len (length lst))
  (define pos (member item lst))
  (if pos
      (- len (length pos))
      #f))

; scales an image to the current canvas size
(define (scale-image img)
  ; width and height of the image
  (define img-width (send img get-width))
  (define img-height (send img get-height))
  
  ; width and height of the canvas
  (define max-width (send ivy-canvas get-width))
  (define max-height (send ivy-canvas get-height))
  
  (cond [(and (> img-width max-width)
              (> img-height max-height))
         (pict->bitmap (scale-to-fit (bitmap img) max-width max-height))]
        [(> img-width max-width)
         (pict->bitmap (scale-to-fit (bitmap img) max-width img-height))]
        [(> img-height max-height)
         (pict->bitmap (scale-to-fit (bitmap img) img-width max-height))]
        [else img]))

; procedure that loads the given image (from the path)
; to the canvas
(define (load-image path)
  (define-values (base name must-be-dir?) (split-path path))
  (set! image-dir base)
  (set! image-path (path->symbol path))
  ; make sure the bitmap loaded correctly
  (define load-success (send image-bmp load-file path))
  (cond [load-success
         (set! image-bmp (scale-image image-bmp))
         ; if we've set tags for this file before...
         (cond [(hash-has-key? master image-path)
                (define tag
                  (string-join (hash-ref master image-path) ","))
                ; ...put them in the tfield
                (send ivy-tag-tfield set-value tag)]
               ; ...otherwise clear the tfield
               [else (send ivy-tag-tfield set-value "")])
         
         (define width (send image-bmp get-width))
         (define height (send image-bmp get-height))
         (send ivy-canvas set-on-paint!
               (λ ()
                 (send ivy-canvas set-canvas-background
                       (make-object color% "black"))
                 (send (send ivy-canvas get-dc) draw-bitmap
                       image-bmp 0 0)))
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
                   (when path (load-image path)))]))

(define ivy-menu-bar-search-tag
  (new menu-item%
       [parent ivy-menu-bar-file]
       [label "&Search Tags"]
       [help-string "Search tags for an image."]
       [callback (λ (i e)
                   (define search-tag-dialog
                     (new dialog%
                          [label "Ivy - Search Tags"]
                          [width 200]
                          [height 100]
                          [style '(close-button)]))
                   (define cancel-button
                     (new button%
                          [parent search-tag-dialog]
                          [callback (λ (button event)
                                      (send search-tag-dialog show #f))]))
                   (define ok-button
                     (new button%
                          [parent search-tag-dialog]
                          [callback (λ (button event)
                                      (send search-tag-dialog show #f))]))
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
                     (define pfs (path-files))
                     (define index (get-index (symbol->path image-path) pfs))
                     (when (and index (> (length pfs) 1))
                       (if (zero? index)
                           (load-image (last pfs))
                           (load-image (list-ref pfs (- index 1)))))))]))

(define ivy-actions-next
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (arrow 15 0))]
       [callback (λ (button event)
                   (unless (eq? image-path '/)
                     (define pfs (path-files))
                     (define index (get-index (symbol->path image-path) pfs))
                     (when (and index (> (length pfs) 1))
                       (if (= index (- (length pfs) 1))
                           (load-image (first pfs))
                           (load-image (list-ref pfs (+ index 1)))))))]))


(define ivy-tag-hpanel (new horizontal-panel%
                            [parent ivy-frame]
                            [stretchable-height #f]))

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
                          (dict-remove! master image-path)]
                         [(not (eq? image-path '/))
                          ; turn the string of tag(s) into a list then sort it
                          (define tag-lst (sort (string-split tag ",") string<?))
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
      (set! do-on-paint thunk))))

(define ivy-canvas
  (new ivy-canvas%
       [parent ivy-frame]
       [label "Ivy Image Canvas"]
       [style '(hscroll vscroll)]
       [paint-callback (λ (canvas dc)
                         (send canvas set-canvas-background
                               (make-object color% "black")))]))
