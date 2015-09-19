#lang racket/gui
; search-results.rkt
(require pict "base.rkt")
(provide results-frame display-tags)

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
       [help-string "Create a collection containing the search results."]
       [callback (λ (button event)
                   (unless (empty? searched-images)
                     (pfs searched-images)
                     (load-image (first searched-images))))]))

(define file-close
  (new menu-item%
       [parent results-menu-bar-file]
       [label "Close"]
       [shortcut #\W]
       [help-string "Close the search results preview."]
       [callback (λ (button event)
                   (send results-frame show #f))]))

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
      (set! do-on-paint thunk))))

(define results-canvas
  (new results-canvas%
       [parent results-frame]
       [style '(vscroll hscroll)]
       [paint-callback (λ (canvas dc)
                         (send canvas set-canvas-background
                               (make-object color% "black")))]))

; search for the tags and display everything
(define (display-tags type tags)
  ; make sure there aren't any nonexistant files in the dictionary
  (clean-dict! master)
  ; do the searching
  (define imgs (apply search-dict master type tags))
  (cond [(empty? imgs)
         (message-box "Ivy - No Images Found"
                      "Sorry! No images with those tags have been found."
                      #f
                      (list 'ok 'stop))]
        [else
         (define imgs-str (sort (map path->string imgs) string<?))
         (set! searched-images (map string->path imgs-str))
         (define imgs-grid (grid-list imgs-str 6))
         
         ; tell the user we're preparing results preview
         (define notification
           (new frame%
                [label "Ivy - Preparing Search Preview"]
                [width 200]
                [height 40]
                [style '(float)]))
         
         (new message%
              [parent notification]
              [label "Preparing search result preview, please wait..."])
         
         (send notification show #t)
         
         ; generate the icon in case it does not exist
         (for ([img-path imgs-str])
           (define str (string-append (string-replace img-path "/" "⁄") ".png"))
           (define icon-path (build-path icons-path str))
           (unless (file-exists? icon-path)
             (generate-icons (list img-path))))
         
         (send notification show #f)
         
         (send results-canvas set-on-paint!
               (λ ()
                 (define dc (send results-canvas get-dc))
                 
                 (send results-canvas set-canvas-background
                       (make-object color% "black"))
                 (for ([img-list imgs-grid]
                       [y (in-naturals)])
                   (for ([path img-list]
                         [x (in-naturals)])
                     (define str (string-append
                                  (string-replace path "/" "⁄") ".png"))
                     (define pct-path (build-path icons-path str))
                     (define pct (bitmap pct-path))
                     (draw-pict pct dc (* 100 x) (* 100 y))))))
         
         (when (positive? (length imgs-str))
           (send results-canvas init-auto-scrollbars #f
                 (* 100 (length imgs-grid)) 0.0 0.0))
         (if (< (length imgs-grid) 4)
             (send results-canvas show-scrollbars #f #f)
             (send results-canvas show-scrollbars #f #t))
         
         (send results-frame show #t)]))
