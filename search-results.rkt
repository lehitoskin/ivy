#lang racket/base
; search-results.rkt
(require pict
         racket/class
         racket/list
         racket/gui/base
         racket/string
         "base.rkt"
         "files.rkt")
(provide results-frame display-tags display-nil-results-alert)

(define searched-images empty)

(define results-frame
  (new frame%
       [label "Ivy - Tag Search Results"]
       [width 650]
       [height 400]))

; set the icon for the frame
(unless (macosx?)
  (send results-frame set-icon (read-bitmap logo)))

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

(define (display-nil-results-alert)
  (message-box "Ivy - No Images Found"
               "Sorry! No images with that tag combination have been found."
               #f
               (list 'ok 'stop)))

; tell the user we're preparing results preview
(define prep-notification
  (new frame%
       [label "Ivy - Preparing Search Preview"]
       [width 200]
       [height 40]
       [style '(float)]))

(define prep-msg
  (new message%
       [parent prep-notification]
       [label "Preparing search result preview, please wait..."]))

; search for the tags and display everything
(define (display-tags imgs)
  (cond [(empty? imgs)
         (display-nil-results-alert)]
        [else
         (send prep-notification show #t)
         
         (define imgs-str (sort (map path->string imgs) string<?))
         (set! searched-images (map string->path imgs-str))
         (define imgs-grid (grid-list imgs-str 6))
         
         ; generate the thumbnail in case it does not exist
         (generate-thumbnails
          (filter path-string?
                  (for/list ([path-str (in-list imgs-str)])
                    (define thumb-name
                      (string-append
                       (if (eq? (system-type) 'windows)
                           (string-replace (string-replace path-str "\\" "_")
                                           "C:" "C")
                           (string-replace path-str "/" "_"))))
                    (define thumb-path (build-path thumbnails-path thumb-name))
                    (if (file-exists? thumb-path)
                        #f
                        path-str))))
         
         (send results-canvas set-on-paint!
               (λ ()
                 (collect-garbage 'incremental)
                 (define dc (send results-canvas get-dc))
                 
                 (send results-canvas set-canvas-background
                       (make-object color% "black"))
                 (for ([img-list (in-list imgs-grid)]
                       [y (in-naturals)])
                   (for ([path-str (in-list img-list)]
                         [x (in-naturals)])
                     (define thumb-name
                       (string-append
                        (if (eq? (system-type) 'windows)
                            (string-replace (string-replace path-str "\\" "_")
                                            "C:" "C")
                            (string-replace path-str "/" "_"))))
                     (define thumb-path (build-path thumbnails-path thumb-name))
                     (define thumb-pct (bitmap thumb-path))
                     (draw-pict thumb-pct dc (* 100 x) (* 100 y))))))
         
         (when (positive? (length imgs-str))
           (send results-canvas init-auto-scrollbars #f
                 (* 100 (length imgs-grid)) 0.0 0.0))
         (if (< (length imgs-grid) 4)
             (send results-canvas show-scrollbars #f #f)
             (send results-canvas show-scrollbars #f #t))
         
         (send prep-notification show #f)
         
         (send results-frame show #t)]))
