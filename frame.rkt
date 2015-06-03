#lang racket/gui
; frame.rkt
; main frame file for ivy, the taggable image viewer
(require "base.rkt" pict)
(provide (all-defined-out))

; path of the currently displayed image
(define image-path '/)
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; pict of the currently displayed image
(define image-pict #f)
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
; img is either a pict or a bitmap%
; type is a symbol
; returns a pict
(define (scale-image img type)
  ; width and height of the image
  (define img-width (if (pict? img)
                        (pict-width img)
                        (send img get-width)))
  (define img-height (if (pict? img)
                         (pict-height img)
                         (send img get-height)))
  
  ; width and height of the canvas
  (define max-width (send ivy-canvas get-width))
  (define max-height (send ivy-canvas get-height))
  
  (case type
    ; might deal with either pict or bitmap% for initial scaling
    [(default)
     (cond [(and (> img-width max-width)
                 (> img-height max-height))
            (scale-to-fit (if (pict? img) img (bitmap img)) max-width max-height)]
           [(> img-width max-width)
            (scale-to-fit (if (pict? img) img (bitmap img)) max-width img-height)]
           [(> img-height max-height)
            (scale-to-fit (if (pict? img) img (bitmap img)) img-width max-height)]
           [else (bitmap img)])]
    ; only used by zoom-in, definitely a pict
    [(larger)
     (scale-to-fit img (* img-width 1.2) (* img-height 1.2))]
    ; only used by zoom-out, definitely a pict
    [(smaller)
     (scale-to-fit img (* img-width 0.8) (* img-height 0.8))]
    [(none) (bitmap img)]))

; procedure that loads the given image to the canvas
(define (load-image img [scale 'default])
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (set! image-dir base)
     (set! image-path (path->symbol img))
     ; make sure the bitmap loaded correctly
     (define load-success (send image-bmp-master load-file img))
     (cond [load-success
            (set! image-pict (scale-image image-bmp-master scale))
            ; if we've set tags for this file before...
            (cond [(hash-has-key? master image-path)
                   (define tag
                     (string-join (hash-ref master image-path) ","))
                   ; ...put them in the tfield
                   (send ivy-tag-tfield set-value tag)]
                  ; ...otherwise clear the tfield
                  [else (send ivy-tag-tfield set-value "")])]
           [else (printf "Error loading file ~a~n" img)])]
    [else
     ; we already have the bitmap
     (set! image-pict (scale-image img scale))])
  
  (define bmp (pict->bitmap image-pict))
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  
  (send ivy-canvas set-on-paint!
        (λ ()
          (define dc (send ivy-canvas get-dc))
          
          (define bmp-center-x (/ width 2))
          (define bmp-center-y (/ height 2))
          (define canvas-center-x (/ (send ivy-canvas get-width) 2))
          (define canvas-center-y (/ (send ivy-canvas get-height) 2))
          
          (send ivy-canvas set-canvas-background
                (make-object color% "black"))
          (send dc draw-bitmap bmp
                (- canvas-center-x bmp-center-x)
                (- canvas-center-y bmp-center-y))))
  (send ivy-canvas init-auto-scrollbars width height 0.0 0.0)
  (send ivy-canvas refresh))

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
                   (define button-hpanel
                     (new horizontal-panel%
                          [parent search-tag-dialog]
                          [alignment '(right center)]))
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

; the pict functions are finicky and need to be done juuuust right
; otherwise the circle is cut off on the right side.
(define ivy-actions-zoom-in
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -12 (circle 15) (text "+ ")))]
       [callback (λ (button event)
                   (load-image image-pict 'larger))]))

(define ivy-actions-zoom-out
  (new button%
       [parent actions-hpanel]
       [label (pict->bitmap (hc-append -10 (circle 15) (text "-  ")))]
       [callback (λ (button event)
                   (load-image image-pict 'smaller))]))

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
