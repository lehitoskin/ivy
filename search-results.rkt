#lang racket/base
; search-results.rkt
(require pict
         racket/class
         racket/gui/base
         racket/list
         racket/path
         racket/string
         "base.rkt"
         "files.rkt")
(provide results-frame display-tags display-nil-results-alert)

(define searched-images empty)

(define results-frame
  (new frame%
       [label "Ivy - Tag Search Results"]
       [width 700]
       [height 450]))

; set the icon for the frame
(unless (macosx?)
  (send results-frame set-icon (read-bitmap logo)))

(define results-menu-bar (new menu-bar% [parent results-frame]))

(define results-menu-bar-file
  (new menu%
       [parent results-menu-bar]
       [label "&File"]))

(define file-open-collection
  (new menu-item%
       [parent results-menu-bar-file]
       [label "&Open as Collection"]
       [shortcut #\O]
       [help-string "Create a collection containing the search results."]
       [callback (λ (button event)
                   (unless (empty? searched-images)
                     (send (ivy-tag-tfield) set-field-background (make-object color% "white"))
                     (pfs searched-images)
                     (load-image (first searched-images))
                     (send results-frame show #f)))]))

(define file-add-to-collection
  (new menu-item%
       [parent results-menu-bar-file]
       [label "Append results to c&ollection"]
       [shortcut #\O]
       [shortcut-prefix (if (macosx?) '(cmd shift) '(ctl shift))]
       [help-string "Append search results to existing collection"]
       [callback (λ (button event)
                   (unless (empty? searched-images)
                     (cond
                       ; empty collection, create a new one
                       [(equal? (first (pfs)) (build-path "/"))
                        (send (ivy-tag-tfield) set-field-background (make-object color% "white"))
                        (pfs searched-images)
                        (load-image (first searched-images))]
                       ; append to the current collection
                       [else
                        (pfs (remove-duplicates (append (pfs) searched-images)))
                        (send (status-bar-position) set-label
                              (format "~a / ~a"
                                      (+ (get-index (image-path) (pfs)) 1)
                                      (length (pfs))))])
                     (send results-frame show #f)))]))

(define file-close
  (new menu-item%
       [parent results-menu-bar-file]
       [label "Close"]
       [shortcut #\W]
       [help-string "Close the search results preview."]
       [callback (λ (button event)
                   (send results-frame show #f))]))

(define txt (new text% [auto-wrap #t]))

(define ecanvas
  (new editor-canvas%
       [parent results-frame]
       [editor txt]
       [style '(auto-hscroll auto-vscroll no-focus)]))
(send ecanvas set-canvas-background (make-object color% "black"))

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
         (send txt erase)
         
         (define imgs-str (sort (map path->string imgs) string<?))
         (set! searched-images (map string->path imgs-str))
         
         (define thumbs-path
           (for/list ([path-str (in-list imgs-str)])
             (define thumb-name
               (string-append
                (if (eq? (system-type) 'windows)
                    (string-replace (string-replace path-str "\\" "_")
                                    "C:" "C")
                    (string-replace path-str "/" "_"))
                ".png"))
             (build-path thumbnails-path thumb-name)))
         
         ; generate the thumbnail in case it does not exist
         (generate-thumbnails
          (filter path-string?
                  (for/list ([thumb (in-list thumbs-path)]
                             [path-str (in-list imgs-str)])
                    (if (file-exists? thumb)
                        #f
                        path-str))))
         
         (for ([thumb-str (in-list thumbs-path)]
               [img-str (in-list imgs-str)])
           (define img-name (path->string (file-name-from-path img-str)))
           (define thumb+name
             (pict->bitmap
              (vc-append
               (bitmap thumb-str)
               (text img-name (list (make-object color% "white"))))))
           (send txt insert (make-object image-snip% thumb+name)))
         
         ; scroll back to the top of the window
         (send txt insert "\n")
         (send txt move-position 'home)
         
         (send prep-notification show #f)
         
         (send results-frame show #t)]))
