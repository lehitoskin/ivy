#lang racket/gui
; search-results.rkt
(require pict
         file/convertible
         "base.rkt")
(provide results-frame
         display-tags)

(define searched-images empty)

(define results-frame
  (new frame%
       [label "Ivy - Tag Search Results"]
       [width 650]
       [height 400]))

(define results-menu-bar (new menu-bar% [parent results-frame]))

(define results-menu-bar-file
  (new menu%
       [parent results-menu-bar]
       [label "&File"]))

(define file-make-virt-dir
  (new menu-item%
       [parent results-menu-bar-file]
       [label "Create virtual directory"]
       [shortcut #\I]
       [help-string "Create a virtual directory containing the search results."]
       [callback (λ (button event)
                   (unless (empty? searched-images)
                     (load-image (car searched-images))
                     (pfs searched-images)))]))

(define file-close
  (new menu-item%
       [parent results-menu-bar-file]
       [label "Close"]
       [shortcut #\W]
       [help-string "Close the tag search results preview."]
       [callback (λ (button event)
                   (send results-frame show #f))]))

; a text% that only allows you to copy text
#|(define mtext%
  (class text%
    (super-new)
    (define/override (on-char kev)
      (define key (send kev get-key-code))
      (case key
        [(control #\c) (send this copy)]
        [(control #\a) (void)]
        #;[else (send this on-default-char kev)]))))

(define results-text (new mtext%))
(send results-text change-style
      (make-object style-delta% 'change-size 10))

(define results-ecanvas
  (new editor-canvas%
       [parent results-frame]
       [editor results-text]
       [style '(auto-hscroll auto-vscroll)]))|#

#;(for ([img (search-dict master "manga")])
    (send results-text insert (path->string img))
    (send results-text insert "\n"))

(define results-canvas%
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
    
    ; each image border is a 100x100 cell
    ; determine which image we are talking about
    ; by finding the closest hundred and then calling
    ; (load-image img)
    #;(define/override (on-event evt)
      (define type (send evt get-event-type))
      (when (eq? type 'left-up)
        (printf "x: ~a, y: ~a~n" (send evt get-x) (send evt get-y))))))

(define results-canvas
  (new results-canvas%
       [parent results-frame]
       [style '(vscroll hscroll)]
       [paint-callback (λ (canvas dc)
                         (send canvas set-canvas-background
                               (make-object color% "black")))]))

; search for the tags and display everything
(define (display-tags type tags)
  (define imgs (apply search-dict master type tags))
  (set! searched-images imgs)
  (unless (empty? imgs)
    (define imgs-str (map path->string imgs))
    (define imgs-grid (grid-list imgs-str 6))
    
    ; generate the icon in case it does not exist
    (for ([img-path imgs-str])
      (define str (string-append (string-replace img-path "/" "⁄") ".png"))
      (define icon-path (build-path icons-path str))
      (unless (file-exists? icon-path)
        (generate-icons (list img-path))))
    
    (send results-canvas set-on-paint!
          (λ ()
            (define dc (send results-canvas get-dc))
            
            (send results-canvas set-canvas-background
                  (make-object color% "black"))
            (for ([img-list imgs-grid]
                  [y (in-naturals)])
              (for ([path img-list]
                    [x (in-naturals)])
                (define str (string-append (string-replace path "/" "⁄") ".png"))
                (define bmp-path (build-path icons-path str))
                (define bmp-port-in (open-input-file bmp-path #:mode 'binary))
                (define bmp (make-object bitmap% bmp-port-in))
                (close-input-port bmp-port-in)
                (send dc draw-bitmap bmp (* 100 x) (* 100 y))))))
    
    (when (positive? (length imgs-str))
      (send results-canvas init-auto-scrollbars #f
            (* 100 (length imgs-grid)) 0.0 0.0))
    (if (< (length imgs-grid) 4)
        (send results-canvas show-scrollbars #f #f)
        (send results-canvas show-scrollbars #f #t))
    
    (send results-frame show #t)))
