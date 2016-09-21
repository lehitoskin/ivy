#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require file/convertible
         gif-image
         pict
         pict/convert
         racket/bool
         racket/class
         racket/function
         racket/gui/base
         racket/list
         racket/path
         racket/string
         (only-in srfi/13
                  string-contains-ci
                  string-null?)
         "db.rkt"
         "files.rkt")
(provide (all-defined-out)
         string-null?)

(define (path->symbol p)
  (string->symbol (path->string p)))

(define (symbol->path p)
  (string->path (symbol->string p)))

(define (macosx?)
  (eq? (system-type) 'macosx))

; path of the currently displayed image
(define image-path (make-parameter (build-path "/")))
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; pict of the currently displayed image
(define image-pict #f)
; directory containing the currently displayed image
(define image-dir (make-parameter (find-system-path 'home-dir)))
(define supported-extensions '("png" "jpg" "jpe" "jpeg" "bmp" "gif"))
(define exact-search? (make-parameter #f))

; animated gif stuff
; list of pict?
(define gif-lst empty)
; list of real?
(define gif-lst-timings empty)
(define gif-thread (make-parameter #f))

; all image files contained within image-dir
(define (path-files)
  (define dir-lst (directory-list (image-dir) #:build? #t))
  (define file-lst
    (for/list ([file dir-lst])
      (define ext (filename-extension file))
      (cond [(false? ext) #f]
            [else
             (define ext-str (string-downcase (bytes->string/utf-8 ext)))
             (if (false? (member ext-str supported-extensions)) #f file)])))
  (filter path? file-lst))
; parameter listof path
; if pfs is empty, attempting to append a single image would
; make pfs just that image, rather than a list of length 1
(define pfs (make-parameter (list (build-path "/"))))

(define (tfield->list tf)
  (define val (send tf get-value))
  (cond [(string-null? val) empty]
        [else
         (define tags
           (filter (negate string-null?)
                   (for/list ([tag (string-split val ",")])
                     (string-trim tag))))
         (remove-duplicates (sort tags string<?))]))

; get index of an item in the list
; numbering starts from 0
(define (get-index item lst)
  (define len (length lst))
  (define pos (member item lst))
  (if pos (- len (length pos)) #f))

; scales an image to the current canvas size
; img is either a pict or a bitmap%
; type is a symbol
; returns a pict
(define (scale-image img type)
  (define canvas (ivy-canvas))
  ; width and height of the image
  (define img-width (if (pict? img)
                        (pict-width img)
                        (send img get-width)))
  (define img-height (if (pict? img)
                         (pict-height img)
                         (send img get-height)))
  
  ; width and height of the canvas
  (define max-width (send canvas get-width))
  (define max-height (send canvas get-height))
  
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
    [(cmd)
     ; canvas is very small before everything is completely loaded
     (set! max-width 800)
     (set! max-height 528)
     (cond [(and (> img-width max-width)
                 (> img-height max-height))
            (scale-to-fit (if (pict? img) img (bitmap img)) max-width max-height)]
           [(> img-width max-width)
            (scale-to-fit (if (pict? img) img (bitmap img)) max-width img-height)]
           [(> img-height max-height)
            (scale-to-fit (if (pict? img) img (bitmap img)) img-width max-height)]
           [else (bitmap img)])]
    ; only used by zoom-in, definitely a pict
    [(larger wheel-larger)
     (scale-to-fit img (* img-width 1.1) (* img-height 1.1))]
    ; only used by zoom-out, definitely a pict
    [(smaller wheel-smaller)
     (scale-to-fit img (* img-width 0.9) (* img-height 0.9))]
    [(same) img]
    [(none) (bitmap img)]
    [else
     (raise-argument-error
      'scale-image
      "(or/c 'default 'cmd 'larger 'wheel-larger 'smaller 'wheel-smaller 'same 'none)"
      type)]))

; janky!
(define ivy-canvas (make-parameter #f))
(define ivy-tag-tfield (make-parameter #f))
(define status-bar-dimensions (make-parameter #f))
(define status-bar-position (make-parameter #f))
(define incoming-tags (make-parameter ""))

(define (animated-gif-callback canvas dc lst)
  (define gif-x (inexact->exact (round (pict-width (first lst)))))
  (define gif-y (inexact->exact (round (pict-height (first lst)))))
  (define gif-center-x (/ gif-x 2))
  (define gif-center-y (/ gif-y 2))
  
  (define canvas-x (send canvas get-width))
  (define canvas-y (send canvas get-height))
  (define canvas-center-x (/ canvas-x 2))
  (define canvas-center-y (/ canvas-y 2))
  
  (define len (length lst))
  
  (let loop ([gif-frame (first lst)]
             [timing (first gif-lst-timings)]
             [i 0])
    
    ; remove any previous frames from the canvas
    (send dc clear)
    
    (cond
      ; if the image is really big, place it at (0,0)
      [(and (> gif-x canvas-x)
            (> gif-y canvas-y))
       (send canvas show-scrollbars #t #t)
       (draw-pict gif-frame dc 0 0)]
      ; if the image is wider than the canvas,
      ; place it at (0,y)
      [(> gif-y canvas-x)
       (send canvas show-scrollbars #t #f)
       (draw-pict gif-frame dc
                  0 (- canvas-center-y gif-center-y))]
      ; if the image is taller than the canvas,
      ; place it at (x,0)
      [(> gif-y canvas-y)
       (send canvas show-scrollbars #f #t)
       (draw-pict gif-frame dc
                  (- canvas-center-x gif-center-x) 0)]
      ; otherwise, place it at the normal position
      [else
       (send canvas show-scrollbars #f #f)
       (draw-pict gif-frame dc
                  (- canvas-center-x gif-center-x)
                  (- canvas-center-y gif-center-y))])
    (sleep timing)
    (if (>= i len)
        (loop (first lst) (first gif-lst-timings) 0)
        (loop (list-ref lst i) (list-ref gif-lst-timings i) (add1 i)))))

; procedure that loads the given image to the canvas
; takes care of updating the dimensions message and
; the position message
; scale: (or/c 'default 'cmd 'larger 'smaller 'none)
(define (load-image img [scale 'default])
  (define canvas (ivy-canvas))
  (define tag-tfield (ivy-tag-tfield))
  (define sbd (status-bar-dimensions))
  (define sbp (status-bar-position))
  
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (image-dir base)
     (image-path img)
     (define img-str (path->string img))
     (cond
       ; load an animated gif
       [(and (gif? img) (gif-animated? img))
        ; make a list of picts
        (set! gif-lst
              (for/list ([bits (gif-images img)])
                (define bmp-in-port (open-input-bytes bits))
                (define bmp (make-object bitmap% 50 50))
                (send bmp load-file bmp-in-port 'gif/alpha)
                (close-input-port bmp-in-port)
                (scale-image bmp scale)))
        (set! gif-lst-timings (gif-timings img))
        (set! image-pict #f)
        (send image-bmp-master load-file img)
        (send sbd set-label
              (format "~a x ~a pixels"
                      (send image-bmp-master get-width)
                      (send image-bmp-master get-height)))]
       ; else load the static image
       [else
        ; make sure the bitmap loaded correctly
        (define load-success (send image-bmp-master load-file img))
        (cond [load-success
               (set! image-pict (scale-image image-bmp-master scale))
               (send sbd set-label
                     (format "~a x ~a pixels"
                             (send image-bmp-master get-width)
                             (send image-bmp-master get-height)))
               (set! gif-lst empty)
               (set! gif-lst-timings empty)]
              [else
               (eprintf "Error loading file ~a~n" img)])])
     
     (send (send canvas get-parent) set-label (path->string name))
     (send sbp set-label
           (format "~a / ~a"
                   (+ (get-index img (pfs)) 1)
                   (length (pfs))))
     
     ; pick what string to display for tags...
     (cond [(db-has-key? 'images img-str)
            (define img-obj (make-data-object sqlc image% img-str))
            (define tags (send img-obj get-tags))
            (incoming-tags (string-join tags ", "))]
           [else (incoming-tags "")])
     ; ...put them in the tfield
     (send tag-tfield set-value (incoming-tags))
     ; ensure the text-field displays the changes we just made
     (send tag-tfield refresh)]
    [(list? img)
     ; scale the image in the desired direction
     (set! gif-lst (map (λ (pct) (scale-image pct scale)) img))]
    [else
     ; we already have the image loaded
     (set! gif-lst empty)
     (set! gif-lst-timings empty)
     (set! image-pict (scale-image img scale))])
  
  (unless (or (false? (gif-thread)) (thread-dead? (gif-thread)))
    (kill-thread (gif-thread)))
  
  (if (not (empty? gif-lst))
      ; gif-lst contains a list of picts, display the animated gif
      (send canvas set-on-paint!
            (λ (canvas dc)
              (unless (or (false? (gif-thread)) (thread-dead? (gif-thread)))
                (kill-thread (gif-thread)))
              
              (send dc set-background "black")
              
              (gif-thread
               (thread
                (λ ()
                  (animated-gif-callback canvas dc gif-lst))))))
      ; otherwise, display the static image
      (send canvas set-on-paint!
            (λ (canvas dc)
              (when (and (path? img) (eq? scale 'default))
                ; have the canvas re-scale the image so when the canvas is
                ; resized, it'll also be the proper size
                (set! image-pict (scale-image image-bmp-master 'default)))
              
              (define img-width (inexact->exact (round (pict-width image-pict))))
              (define img-height (inexact->exact (round (pict-height image-pict))))
              
              (define img-center-x (/ img-width 2))
              (define img-center-y (/ img-height 2))
              (define canvas-x (send canvas get-width))
              (define canvas-y (send canvas get-height))
              (define canvas-center-x (/ canvas-x 2))
              (define canvas-center-y (/ canvas-y 2))
              
              ; keep the background black
              (send canvas set-canvas-background
                    (make-object color% "black"))
              
              ; alleviate image "jaggies"
              (define bmp (pict->bitmap image-pict))
              
              (cond
                ; if the image is really big, place it at (0,0)
                [(and (> img-width canvas-x)
                      (> img-height canvas-y))
                 (send canvas show-scrollbars #t #t)
                 (send dc draw-bitmap bmp 0 0)]
                ; if the image is wider than the canvas,
                ; place it at (0,y)
                [(> img-width canvas-x)
                 (send canvas show-scrollbars #t #f)
                 (send dc draw-bitmap bmp
                       0 (- canvas-center-y img-center-y))]
                ; if the image is taller than the canvas,
                ; place it at (x,0)
                [(> img-height canvas-y)
                 (send canvas show-scrollbars #f #t)
                 (send dc draw-bitmap bmp
                       (- canvas-center-x img-center-x) 0)]
                ; otherwise, place it at the normal position
                [else
                 (send canvas show-scrollbars #f #f)
                 (send dc draw-bitmap bmp
                       (- canvas-center-x img-center-x)
                       (- canvas-center-y img-center-y))]))))
  
  ; tell the scrollbars to adjust for the size of the image
  (let ([img-x (inexact->exact (round (pict-width (if image-pict image-pict (first gif-lst)))))]
        [img-y (inexact->exact (round (pict-height (if image-pict image-pict (first gif-lst)))))])
    ; will complain if width/height is less than 1
    (define width (if (< img-x 1) 1 img-x))
    (define height (if (< img-y 1) 1 img-y))
    (define-values (virtual-x virtual-y) (send canvas get-virtual-size))
    
    (case scale
      ; zoom with the center of the current center
      [(smaller larger)
       (define-values (client-x client-y) (send canvas get-client-size))
       (define client-center-x (/ client-x 2))
       (define client-center-y (/ client-y 2))
       (send canvas init-auto-scrollbars width height
             (exact->inexact (/ client-center-x virtual-x))
             (exact->inexact (/ client-center-y virtual-y)))]
      ; place scrollbars on mouse location
      [(wheel-smaller wheel-larger)
       (define-values (mouse-x mouse-y) (send canvas get-mouse-pos))
       ; coordinates of top left corner of visible section of the virtual canvas
       (define-values (visible-x visible-y) (send canvas get-view-start))
       ; position of mouse over the entire displayed image
       (define mouse/visible-x (if (> mouse-x virtual-x)
                                   virtual-x
                                   (+ mouse-x visible-x)))
       (define mouse/visible-y (if (> mouse-y virtual-y)
                                   virtual-y
                                   (+ mouse-y visible-y)))
       (send canvas init-auto-scrollbars width height
             (exact->inexact (/ mouse/visible-x virtual-x))
             (exact->inexact (/ mouse/visible-y virtual-y)))]
      [else
       ; otherwise just set it to the top left corner
       (send canvas init-auto-scrollbars width height 0.0 0.0)]))
  (send canvas refresh))

; curried procedure to abstract loading an image in a collection
; mmm... curry
(define ((load-image-in-collection direction))
  (unless (eq? (path->symbol (image-path)) '/)
    ; kill the gif thread, if applicable
    (unless (or (false? (gif-thread)) (thread-dead? (gif-thread)))
      (kill-thread (gif-thread)))
    (send (ivy-tag-tfield) set-field-background (make-object color% "white"))
    (define prev-index (get-index (image-path) (pfs)))
    (case direction
      [(previous)
       (when (and prev-index (> (length (pfs)) 1))
         (cond [(zero? prev-index)
                (define img (last (pfs))) ; this is a path
                (load-image img)]
               [else
                (define img (list-ref (pfs) (- prev-index 1)))
                (load-image img)]))]
      [(next)
       (when (and prev-index (> (length (pfs)) 1))
         (cond [(= prev-index (- (length (pfs)) 1))
                (define img (first (pfs))) ; this is a path
                (load-image img)]
               [else
                (define img (list-ref (pfs) (+ prev-index 1)))
                (load-image img)]))]
      [(rand) (load-image (list-ref (pfs) (- (random 1 (+ (length (pfs)) 1)) 1)))]
      [(home) (load-image (first (pfs)))]
      [(end) (load-image (last (pfs)))])))

; is this complicating things? I have no idea, but we should never
; underestimate the `cool' factor
(define load-previous-image (load-image-in-collection 'previous))
(define load-next-image (load-image-in-collection 'next))
(define load-rand-image (load-image-in-collection 'rand))
(define load-first-image (load-image-in-collection 'home))
(define load-last-image (load-image-in-collection 'end))

; takes a list lst and a width
; returns a list of lists of lengths no more than width
(define (grid-list lst width [accum empty])
  (if (> width (length lst))
      (filter (negate empty?) (append accum (list lst)))
      (grid-list (drop lst width) width (append accum (list (take lst width))))))

(define (path->thumb-path path)
  (define path-str (if (path? path) (path->string path) path))
  (define thumb-name
    (string-append
     (if (eq? (system-type) 'windows)
         (string-replace (string-replace path-str "\\" "_")
                         "C:" "C")
         (string-replace path-str "/" "_"))
     ".png"))
  (build-path thumbnails-path thumb-name))

; generates 100x100 thumbnails from a list of string paths
; e.g. (generate-thumbnails (map path->string (search-dict master 'or "beach")))
(define (generate-thumbnails imgs)
  (for ([path (in-list imgs)])
    ; create and load the bitmap
    (define thumb-bmp (read-bitmap path))
    (define thumb-path (path->thumb-path path))
    ; use pict to scale the image to 100x100
    (define thumb-pct (bitmap thumb-bmp))
    (define thumb-small (pict->bitmap (scale-to-fit thumb-pct 100 100)))
    (define thumb-port-out (open-output-file thumb-path
                                             #:mode 'binary
                                             #:exists 'truncate/replace))
    (printf "Writing bytes to ~a~n" thumb-path)
    (write-bytes (convert thumb-small 'png-bytes) thumb-port-out)
    (close-output-port thumb-port-out)))

; implement common keyboard shortcuts
(define (init-editor-keymap km)
  
  (send km add-function "insert-clipboard"
        (lambda (editor kev)
          (send editor paste)))
  
  (send km add-function "insert-primary"
        (lambda (editor kev)
          (send editor paste-x-selection)))
  
  (send km add-function "cut"
        (lambda (editor kev)
          (send editor cut)))
  
  (send km add-function "copy"
        (lambda (editor kev)
          (send editor copy)))
  
  (send km add-function "select-all"
        (lambda (editor kev)
          (send editor move-position 'end)
          (send editor extend-position 0)))
  
  (send km add-function "delete-backward-char"
        (lambda (editor kev)
          (send editor delete)))
  
  (send km add-function "delete-forward-char"
        (lambda (editor kev)
          (send editor delete
                (send editor get-start-position)
                (+ (send editor get-end-position) 1))))
  
  (send km add-function "backward-char"
        (lambda (editor kev)
          (send editor move-position 'left)))
  
  (send km add-function "backward-word"
        (lambda (editor kev)
          (send editor move-position 'left #f 'word)))
  
  (send km add-function "backward-kill-word"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'left #f 'word)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-word"
        (lambda (editor kev)
          (define from (send editor get-start-position))
          (send editor move-position 'right #f 'word)
          (define to (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-buffer"
        (lambda (editor kev)
          (send editor kill)))
  
  (send km add-function "backward-kill-buffer"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'home)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "mark-char-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'simple))))
  
  (send km add-function "mark-char"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'simple))))
  
  (send km add-function "mark-word-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'word))))
  
  (send km add-function "mark-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "mark-whole-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #f 'word)
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "forward-char"
        (lambda (editor kev)
          (send editor move-position 'right)))
  
  (send km add-function "forward-word"
        (lambda (editor kev)
          (send editor move-position 'right #f 'word)))
  
  (send km add-function "beginning-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'home)))
  
  (send km add-function "end-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'end)))
  
  ; from gui-lib/mred/private/editor.rkt
  (send km add-function "mouse-popup-menu"
        (lambda (edit event)
          (when (send event button-up?)
            (let ([a (send edit get-admin)])
              (when a
                (let ([m (make-object popup-menu%)])
                  (append-editor-operation-menu-items m)
                  ;; Remove shortcut indicators (because they might not be correct)
                  (for-each
                   (lambda (i)
                     (when (is-a? i selectable-menu-item<%>)
                       (send i set-shortcut #f)))
                   (send m get-items))
                  (let-values ([(x y) (send edit
                                            dc-location-to-editor-location
                                            (send event get-x)
                                            (send event get-y))])
                    (send a popup-menu m (+ x 5) (+ y 5)))))))))
  km)

(define (set-default-editor-bindings km)
  (cond [(macosx?)
         (send km map-function ":d:c" "copy")
         (send km map-function ":d:с" "copy") ;; russian cyrillic
         (send km map-function ":d:v" "insert-clipboard")
         (send km map-function ":d:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":d:x" "cut")
         (send km map-function ":d:ч" "cut") ;; russian cyrillic
         (send km map-function ":d:a" "select-all")
         (send km map-function ":d:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":d:backspace" "backward-kill-buffer")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":a:left" "backward-word")
         (send km map-function ":a:right" "forward-word")
         (send km map-function ":a:backspace" "backward-kill-word")
         (send km map-function ":a:delete" "forward-kill-word")
         (send km map-function ":c:k" "forward-kill-buffer")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":a:s:left" "mark-word-backward")
         (send km map-function ":a:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":d:left" "beginning-of-buffer")
         (send km map-function ":c:a" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":d:right" "end-of-buffer")
         (send km map-function ":c:e" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":leftbuttondouble" "mark-whole-word")]
        [else
         (send km map-function ":c:c" "copy")
         (send km map-function ":c:с" "copy") ;; russian cyrillic
         (send km map-function ":c:v" "insert-clipboard")
         (send km map-function ":c:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":c:x" "cut")
         (send km map-function ":c:ч" "cut") ;; russian cyrillic
         (send km map-function ":c:a" "select-all")
         (send km map-function ":c:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":c:left" "backward-word")
         (send km map-function ":c:right" "forward-word")
         (send km map-function ":c:backspace" "backward-kill-word")
         (send km map-function ":c:delete" "forward-kill-word")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":c:s:left" "mark-word-backward")
         (send km map-function ":c:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":middlebutton" "insert-primary")
         (send km map-function ":leftbuttondouble" "mark-whole-word")
         (send km map-function ":s:insert" "insert-primary")]))

(current-text-keymap-initializer
 (λ (keymap)
   (define km (init-editor-keymap keymap))
   (set-default-editor-bindings km)))
