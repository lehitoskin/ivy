#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require file/convertible
         file/sha1
         gif-image
         pict
         racket/bool
         racket/class
         racket/contract
         racket/file
         racket/format
         racket/function
         racket/gui/base
         racket/list
         racket/path
         racket/string
         riff
         rsvg
         (only-in srfi/13
                  string-contains-ci
                  string-null?)
         "db.rkt"
         "embed.rkt"
         "files.rkt")
(provide (all-defined-out)
         string-null?
         gif?
         gif-animated?)

(define (path->symbol p)
  (string->symbol (path->string p)))

(define (symbol->path p)
  (string->path (symbol->string p)))

(define (macosx?)
  (eq? (system-type) 'macosx))

(define root-path
  (if (eq? (system-type) 'windows)
      (build-path "C:\\")
      (build-path "/")))
; path of the currently displayed image
(define image-path (make-parameter root-path))
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; pict of the currently displayed image
(define image-pict #f)
; the cached XMP metadata of the image
(define image-xmp (box empty))
(define xmp-threads (make-hash))
; bitmap to actually display
; eliminate image "jaggies"
; reduce amount of times we use pict->bitmap, as this takes a very long time
(define image-bmp (make-bitmap 50 50))
; directory containing the currently displayed image
(define image-dir (make-parameter (find-system-path 'home-dir)))
; the only extensions ivy will accept - ignores everything else
(define supported-extensions '(".bmp"
                               ".flif"
                               ".gif"
                               ".jpe"
                               ".jpeg"
                               ".JPEG"
                               ".jpg"
                               ".JPG"
                               ".png"
                               ".svg"
                               ".xbm"
                               ".xpm"))
; gif/flif stuff
; listof pict?
(define image-lst-master empty)
(define image-lst empty)
; listof real?
(define image-lst-timings empty)
; number of times to loop a FLIF (0 = forever)
(define image-num-loops 0)
(define animation-thread (make-parameter #f))
(define decoder-thread (make-parameter #f))
; number from the decoder thread that shows how much the image is loaded
(define flif-load-progress (box 0))
; GIF cumulative animation
(define cumulative? (make-parameter #f))
(define exact-search? (make-parameter #f))

(define color-white (make-object color% "white"))
(define color-black (make-object color% "black"))
(define color-spring-green (make-object color% "spring green"))
(define color-gold (make-object color% "gold"))

; contract for image scaling
(define image-scale/c
  (or/c 'default
        'cmd
        'larger
        'wheel-larger
        'smaller
        'wheel-smaller
        'same
        'none
        10
        20
        30
        40
        50
        60
        70
        80
        90
        100))

(define/contract (supported-file? img)
  (path-string? . -> . boolean?)
  (define ext (path-get-extension img))
  (and ext
       (member (string-downcase (bytes->string/utf-8 ext)) supported-extensions)
       #t))

; find all supported images in dir,
; searched recursively
(define (dir-files [dir (image-dir)])
  (find-files supported-file? dir))

; all image files contained within image-dir,
; but not recursively searched
(define (path-files)
  (define dir-lst (directory-list (image-dir) #:build? #t))
  (filter supported-file? dir-lst))

; parameter listof path
; if pfs is empty, attempting to append a single image would
; make pfs just that image, rather than a list of length 1
(define pfs (make-parameter (list root-path)))

(define (string->taglist str)
  (cond [(string-null? str) empty]
        [else
         (define tags
           (filter (λ (tag) (not (string-null? tag)))
                   (for/list ([tag (string-split str ",")])
                     (string-trim tag))))
         (remove-duplicates (sort tags string<?))]))

(define/contract (tfield->list tf)
  ((is-a?/c text-field%). -> . list?)
  (define val (send tf get-value))
  (string->taglist val))

; get index of an item in the list
; numbering starts from 0
(define/contract (get-index item lst)
  (any/c list? . -> . (or/c integer? false?))
  (define len (length lst))
  (define pos (member item lst))
  (if pos (- len (length pos)) #f))

(define (string-truncate str n)
  (if (<= (string-length str) n)
      str
      (substring str 0 n)))

; awww yeah... so oldskool...
(define (remove-children parent kids)
  (when (> (length kids) 0)
    (send parent delete-child (car kids))
    (remove-children parent (cdr kids))))

; just check out those tail recursions...
(define (add-children parent kids)
  (when (> (length kids) 0)
    (send parent add-child (car kids))
    (add-children parent (cdr kids))))

;; COPIED FROM opengl/main
;; Convert argb -> rgba
;; Modern wisdom is not to convert to rgba but rather use
;; GL_BGRA with GL_UNSIGNED_INT_8_8_8_8_REV. But that turns out not
;; to work on some implementations, even ones which advertise
;; OpenGL 1.2 support. Great.
(define (argb->rgba! pixels)
  (for ([i (in-range (/ (bytes-length pixels) 4))])
    (let* ([offset (* 4 i)]
           [alpha (bytes-ref pixels (+ 0 offset))]
           [red   (bytes-ref pixels (+ 1 offset))]
           [green (bytes-ref pixels (+ 2 offset))]
           [blue  (bytes-ref pixels (+ 3 offset))])
      (bytes-set! pixels (+ 0 offset) red)
      (bytes-set! pixels (+ 1 offset) green)
      (bytes-set! pixels (+ 2 offset) blue)
      (bytes-set! pixels (+ 3 offset) alpha))))
;; </COPIED>

(define (rgba->argb! pixels)
  (for ([i (in-range (/ (bytes-length pixels) 4))])
    (let* ([offset (* 4 i)]
           [red   (bytes-ref pixels (+ 0 offset))]
           [green (bytes-ref pixels (+ 1 offset))]
           [blue  (bytes-ref pixels (+ 2 offset))]
           [alpha (bytes-ref pixels (+ 3 offset))])
      (bytes-set! pixels (+ 0 offset) alpha)
      (bytes-set! pixels (+ 1 offset) red)
      (bytes-set! pixels (+ 2 offset) green)
      (bytes-set! pixels (+ 3 offset) blue))))

(define (flif->list dec-ptr)
  ; only look at the first frame if we want a static image
  (define num (if (want-animation?)
                  (flif-decoder-num-images dec-ptr)
                  1))
  (for/list ([i (in-range num)])
    (define image (flif-decoder-get-image dec-ptr i))
    (define width (flif-image-get-width image))
    (define height (flif-image-get-height image))
    ; make sure to decode with the proper depth
    (define reader (if (= (flif-image-get-depth image) 8)
                       flif-image-read-rgba8
                       flif-image-read-rgba16))
    (define pixels (reader image width height))
    (rgba->argb! pixels)
    (define bitmap (make-object bitmap% width height))
    (send bitmap set-argb-pixels 0 0 width height pixels)
    bitmap))

(define (progressive-callback quality num-read)
  (define lst (flif->list (decoder)))
  (set! image-bmp-master (first lst))
  (cond [(and (want-animation?) (> (length lst) 1))
         (set! image-lst-timings
               (let ([image (flif-decoder-get-image (decoder) 0)])
                 (make-list (length lst) (flif-image-get-frame-delay image))))
         (load-image (map bitmap lst) 'default)]
        [else (load-image (first lst) 'default)])
  ; set the new frame label
  ;(define-values (base name must-be-dir?) (split-path (image-path)))
  #;(send (send (ivy-canvas) get-parent)
        set-label
        (format "(~a%) ~a" (exact->inexact (/ quality 100)) (path->string name)))
  ; set the load progress
  ;(set-box! flif-load-progress quality)
  ; the fewer the calls, the faster the total decoding
  (+ quality 5000))

; objects that will be used extensively in transparency-grid
(define dgray-color (make-object color% 128 128 128))
(define lgray-color (make-object color% 204 204 204))
(define dgray-square (filled-rectangle 10 10 #:color dgray-color #:draw-border? #f))
(define lgray-square (filled-rectangle 10 10 #:color lgray-color #:draw-border? #f))

; creates a dark-gray/light-gray grid to place behind images
; so that if they are transparent, the grid will become visible.
(define/contract (transparency-grid img)
  ((or/c (is-a?/c bitmap%) pict?) . -> . pict?)
  (define x (if (pict? img) (pict-width img) (send img get-width)))
  (define y (if (pict? img) (pict-height img) (send img get-height)))
  ; 10 for a square's width
  (define x-times (inexact->exact (floor (/ x 10))))
  ; 20 for both square's height
  (define y-times (inexact->exact (floor (/ y 20))))
  (define remainder-x (modulo (floor x) 10))
  (define remainder-y (modulo (floor y) 20))
  ; remainder square for width
  (define rdsquare-x
    (if (> remainder-x 0)
        (filled-rectangle remainder-x 10 #:color dgray-color #:draw-border? #f)
        #f))
  (define rlsquare-x
    (if (> remainder-x 0)
        (filled-rectangle remainder-x 10 #:color lgray-color #:draw-border? #f)
        #f))
  ; remainder square for height
  (define rdsquare-y
    (if (> remainder-y 0)
        (filled-rectangle 10 (if (> remainder-y 10)
                                 (- remainder-y 10)
                                 remainder-y) #:color dgray-color #:draw-border? #f)
        #f))
  (define rlsquare-y
    (if (> remainder-y 0)
        (filled-rectangle 10 (if (> remainder-y 10)
                                 (- remainder-y 10)
                                 remainder-y) #:color lgray-color #:draw-border? #f)
        #f))
  ; remainder square for both width and height
  (define rdsquare-xy
    (cond
      [(> remainder-x 0)
       (if (> remainder-y 0)
           (if (> remainder-y 10)
               (filled-rectangle remainder-x
                                 (- remainder-y 10)
                                 #:color dgray-color
                                 #:draw-border? #f)
               (filled-rectangle remainder-x
                                 remainder-y
                                 #:color dgray-color
                                 #:draw-border? #f))
           #f)]
      [else #f]))
  (define rlsquare-xy
    (cond
      [(> remainder-x 0)
       (if (> remainder-y 0)
           (filled-rectangle remainder-x (if (> remainder-y 10)
                                             (- remainder-y 10)
                                             remainder-y) #:color lgray-color #:draw-border? #f)
           #f)]
      [else #f]))
  ; normal dgray-lgray line
  (define aline
    (let ([aline
           (for/list ([times (in-range x-times)]
                      [i (in-naturals)])
             (if (even? i)
                 dgray-square
                 lgray-square))])
      (if (> remainder-x 0)
          (if (even? x-times)
              (apply hc-append (flatten (append aline (if rdsquare-x rdsquare-x empty))))
              (apply hc-append (flatten (append aline (if rlsquare-x rlsquare-x empty)))))
          (apply hc-append aline))))
  ; normal lgray-dgray line
  (define bline
    (let ([bline
           (for/list ([times (in-range x-times)]
                      [i (in-naturals)])
             (if (even? i)
                 lgray-square
                 dgray-square))])
      (if (> remainder-x 0)
          (if (even? x-times)
              (apply hc-append (flatten (append bline (if rlsquare-x rlsquare-x empty))))
              (apply hc-append (flatten (append bline (if rdsquare-x rdsquare-x empty)))))
          (apply hc-append bline))))
  ; remainder-sized line for both x and y
  ; if there is no remainder for x or y, don't add them
  (define raline
    (let ([aline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rdsquare-y rdsquare-y empty)
                  (if rlsquare-y rlsquare-y empty))))]
          [bline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rlsquare-y rlsquare-y empty)
                  (if rdsquare-y rdsquare-y empty))))])
      (if (even? x-times)
          (apply hc-append (flatten (append aline (if rlsquare-xy rlsquare-xy empty))))
          (apply hc-append (flatten (append bline (if rdsquare-xy rdsquare-xy empty)))))))
  (define rbline
    (let ([aline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rdsquare-y rdsquare-y empty)
                  (if rlsquare-y rlsquare-y empty))))]
          [bline
           (flatten
            (for/list ([times (in-range x-times)]
                       [i (in-naturals)])
              (if (even? i)
                  ; rds-y or rls-y may be #f
                  (if rlsquare-y rlsquare-y empty)
                  (if rdsquare-y rdsquare-y empty))))])
      (if (even? x-times)
          (apply hc-append (flatten (append bline (if rlsquare-xy rlsquare-xy empty))))
          (apply hc-append (flatten (append aline (if rdsquare-xy rdsquare-xy empty)))))))
  ; put it all together
  (let ([base-grid (make-list y-times (vl-append aline bline))])
    (cond [(> remainder-x 0)
           (if (even? x-times)
               (apply vl-append (flatten (append base-grid (if (> remainder-y 0)
                                                               (if (> remainder-y 10)
                                                                   (list aline rbline)
                                                                   raline)
                                                               rbline))))
               (apply vl-append (flatten (append base-grid (if (> remainder-y 0)
                                                               (if (> remainder-y 10)
                                                                   (list aline raline)
                                                                   rbline)
                                                               raline)))))]
          [(> remainder-y 0)
           (if (> remainder-y 10)
               (apply vl-append
                      (append base-grid (list aline (if (even? x-times) rbline raline))))
               (apply vl-append
                      (append base-grid (list raline))))]
          [else (apply vl-append base-grid)])))

(define/contract (transparency-grid-append img)
  ((or/c (is-a?/c bitmap%) pict?) . -> . pict?)
  (define x (if (pict? img) (pict-width img) (send img get-width)))
  (define pct (if (pict? img) img (bitmap img)))
  (define grid (transparency-grid img))
  ; generated grid size is imperfect
  (define offset (- x (pict-width grid)))
  (if (= offset 0)
      (hc-append (- x) grid pct)
      (hc-append (- offset x) grid pct)))

; scales an image to the current canvas size
; img is either a pict or a bitmap%
; type is a symbol
; returns a pict
(define/contract (scale-image img type)
  ((or/c (is-a?/c bitmap%) pict?) image-scale/c . -> . pict?)
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
     ; these are the effective dimensions of the canvas
     (set! max-width 800)
     (set! max-height 516)
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
    [(10 20 30 40 50 60 70 80 90 100)
     (define num (/ type 100))
     (scale-to-fit img (* img-width num) (* img-height num))]
    [(none) (bitmap img)]))

; janky!
(define ivy-canvas (make-parameter #f))
(define ivy-tag-tfield (make-parameter #f))
(define ivy-actions-rating (make-parameter #f))
(define status-bar-dimensions (make-parameter #f))
(define status-bar-error (make-parameter #f))
(define status-bar-position (make-parameter #f))
(define status-bar-size (make-parameter #f))
(define incoming-tags (make-parameter ""))
(define want-animation? (make-parameter #f))

(define/contract (animation-callback canvas dc lst)
  ((is-a?/c canvas%) (is-a?/c dc<%>) list? . -> . void?)
  (define img-x (inexact->exact (round (pict-width (first lst)))))
  (define img-y (inexact->exact (round (pict-height (first lst)))))
  (define img-center-x (/ img-x 2))
  (define img-center-y (/ img-y 2))
  
  (define canvas-x (send canvas get-width))
  (define canvas-y (send canvas get-height))
  (define canvas-center-x (/ canvas-x 2))
  (define canvas-center-y (/ canvas-y 2))
  
  (define len (length lst))

  ; the left and top offsets for each frame in the gif,
  ; just in case the frames are of varying sizes
  (define left/top
    (cond
      [(gif? (image-path))
       (for/list ([bit-frame (gif-images (image-path))]
                  [master-frame (in-list image-lst-master)]
                  [gif-frame (in-list image-lst)])
         (define master-width (pict-width master-frame))
         (define master-height (pict-height master-frame))
         (define gif-width (pict-width gif-frame))
         (define gif-height (pict-height gif-frame))
         ; grab the frame's image descriptor
         ; starting with the graphics control extension
         (define matched (car (regexp-match-positions (byte-regexp (bytes #x21 #xf9)) bit-frame)))
         (define lengths (subbytes bit-frame (+ (car matched) 9) (+ (car matched) 13)))
         (define left (bytes (bytes-ref lengths 1) (bytes-ref lengths 0)))
         (define top (bytes (bytes-ref lengths 3) (bytes-ref lengths 2)))
         (list (* (string->number (bytes->hex-string left) 16) (/ gif-width master-width))
               (* (string->number (bytes->hex-string top) 16) (/ gif-height master-height))))]
      [else (make-list len (list 0 0))]))
  
  ; determine x and y placement as well as
  ; modify the scrollbars outside the animation loop
  (define x-loc 0)
  (define y-loc 0)
  (cond
    ; if the image is really big, place it at (0,0)
    [(and (> img-x canvas-x)
          (> img-y canvas-y))
     ; x-loc and y-loc are already 0
     (send canvas show-scrollbars #t #t)]
    ; if the image is wider than the canvas, place it at (0,y)
    [(> img-x canvas-x)
     (send canvas show-scrollbars #t #f)
     ; x-loc is already 0
     (set! y-loc (- canvas-center-y img-center-y))]
    ; if the image is taller than the canvas, place it at (x,0)
    [(> img-y canvas-y)
     (send canvas show-scrollbars #f #t)
     ; y-loc is already 0
     (set! x-loc (- canvas-center-x img-center-x))]
    ; otherwise, place it at the center of the canvas
    [else
     (send canvas show-scrollbars #f #f)
     (set! x-loc (- canvas-center-x img-center-x))
     (set! y-loc (- canvas-center-y img-center-y))])
  
  ; actual animation loop
  ; runs until animation-thread is killed
  (let loop ([img-frame (first lst)]
             [timing
              (if (empty? image-lst-timings)
                  1/20
                  (first image-lst-timings))]
             [offsets (first left/top)]
             [i 0]
             [times 0])
    ; remove any previous frames from the canvas
    (unless (cumulative?) (send dc clear))
    (draw-pict img-frame dc (+ x-loc (first offsets)) (+ y-loc (second offsets)))
    (sleep timing)
    (cond
      ; loop forever
      ; ran through every frame
      [(and (>= i len) (= image-num-loops 0))
       (loop (first lst) (if (empty? image-lst-timings)
                             1/20
                             (first image-lst-timings))
             (first left/top) 0 0)]
      ; loop forever
      ; still need to display the other frames
      [(and (not (>= i len)) (= image-num-loops 0))
       (loop (list-ref lst i)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (list-ref image-lst-timings i))
             (list-ref left/top i)
             (add1 i)
             0)]
      ; stop looping eventually
      ; ran through every frame, increment times
      [(and (>= i len) (not (= image-num-loops 0)))
       (loop (first lst)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (first image-lst-timings))
             (first left/top)
             0
             (add1 times))]
      ; stop looping eventually
      ; still need to display the other frames
      [(and (not (>= i len))
            (not (= image-num-loops 0)))
       (loop (list-ref lst i)
             ; hacky because decoding a FLIF is very slow right now
             (if (empty? image-lst-timings)
                 1/20
                 (list-ref image-lst-timings i))
             (list-ref left/top i)
             (add1 i)
             times)]
      ; reached the end of our allowed loops, do nothing
      [(and (>= i len) (not (= image-num-loops 0))) #t]
      [else #t])))

; procedure that loads the given image to the canvas
; takes care of updating the dimensions message and
; the position message
(define/contract (load-image img [scale 'default])
  (->* ([or/c path? pict? (is-a?/c bitmap%) (listof pict?)])
       (image-scale/c)
       void?)
  (define canvas (ivy-canvas))
  (define tag-tfield (ivy-tag-tfield))
  (define iar (ivy-actions-rating))
  (define sbd (status-bar-dimensions))
  (define sbe (status-bar-error))
  (define sbp (status-bar-position))
  
  (send sbe set-label "")
  
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (image-dir base)
     (image-path img)
     (define img-str (path->string img))
     (cond
       ; load an animated gif
       [(and (want-animation?) (gif? img) (gif-animated? img))
        (define load-success (send image-bmp-master load-file img))
        ; set the new frame label
        (send (send canvas get-parent) set-label (path->string name))
        ; make a list of picts
        (with-handlers
            ([exn:fail? (λ (e)
                          (eprintf "Error loading animated gif ~v: ~a\n"
                                   (path->string name)
                                   (exn-message e))
                          ; set the gifs to defaults
                          (set! image-lst-master empty)
                          (set! image-lst empty)
                          (set! image-lst-timings empty)
                          ; just load the static image instead
                          (load-image (bitmap img))
                          (send sbe set-label
                                (format "Error loading file ~v"
                                        (string-truncate (path->string name) 30))))])
          (cumulative? (gif-cumulative? img))
          (define lst
            (for/list ([bits (gif-images img)])
              (define bmp-in-port (open-input-bytes bits))
              (define bmp (make-object bitmap% 50 50))
              (send bmp load-file bmp-in-port 'gif/alpha)
              (close-input-port bmp-in-port)
              (bitmap bmp)))
          (set! image-lst-master lst)
          (set! image-lst (map (λ (gif-frame) (scale-image gif-frame scale)) lst))
          (set! image-lst-timings (gif-timings img))
          (set! image-pict #f))
        (define size (file-size (image-path)))
        (send sbd set-label
              (format "~a x ~a pixels  ~a"
                      (send image-bmp-master get-width)
                      (send image-bmp-master get-height)
                      (cond [(>= size (expt 2 20))
                             (format "~a MiB"
                                     (~r (exact->inexact (/ size (expt 2 20)))
                                         #:precision 1))]
                            [(>= size (expt 2 10))
                             (format "~a KiB"
                                     (~r (exact->inexact (/ size (expt 2 10)))
                                         #:precision 1))]
                            [else
                             (format "~a B" size)])))
        (send sbp set-label
              (format "~a / ~a"
                      (+ (get-index img (pfs)) 1)
                      (length (pfs))))]
       ; load animated flif
       [(and (want-animation?) (flif? img) (flif-animated? img))
        (cumulative? #f)
        (decoder (flif-create-decoder))
        ; progressive decoding
        (flif-decoder-set-callback! (decoder) progressive-callback)
        (flif-decoder-set-first-callback-quality! (decoder) 10000)
        ; put the actual decoding in its own thread
        #;(decoder-thread
         (thread (λ ()
                   ; set the progress to 0
                   (set-box! flif-load-progress 0)
                   ; decode, but do not immediately destroy the decoder
                   (flif-decoder-decode-file! (decoder) img)
                   (define num-frames (flif-decoder-num-images (decoder)))
                   (define image (flif-decoder-get-image (decoder) 0))
                   (set! image-lst-timings
                         (make-list num-frames
                                    (/ (flif-image-get-frame-delay image) 1000)))
                   (set! image-num-loops (flif-decoder-num-loops (decoder))))))
        ; regular decoding
        (flif-decoder-decode-file! (decoder) img)
        (let ([image (flif-decoder-get-image (decoder) 0)]
              [num-frames (flif-decoder-num-images (decoder))])
          (set! image-lst-timings
                (make-list num-frames
                           (/ (flif-image-get-frame-delay image) 1000)))
          (set! image-num-loops (flif-decoder-num-loops (decoder))))
        ; set the new frame label
        (send (send canvas get-parent) set-label (path->string name))
        ; set the gui information
        (define size (file-size (image-path)))
        (define dimensions (flif-dimensions (image-path)))
        (send sbd set-label
              (format "~a x ~a pixels  ~a"
                      (first dimensions)
                      (second dimensions)
                      (cond [(>= size (expt 2 20))
                             (format "~a MiB"
                                     (~r (exact->inexact (/ size (expt 2 20)))
                                         #:precision 1))]
                            [(>= size (expt 2 10))
                             (format "~a KiB"
                                     (~r (exact->inexact (/ size (expt 2 10)))
                                         #:precision 1))]
                            [else
                             (format "~a B" size)])))
        (send sbp set-label
              (format "~a / ~a"
                      (+ (get-index img (pfs)) 1)
                      (length (pfs))))]
       ; else load the static image
       [else
        ; make sure the bitmap loaded correctly
        (define load-success
          (cond [(bytes=? (path-get-extension img) #".svg")
                 (and (set! image-bmp-master (load-svg-from-file img)) #t)]
                [(flif? img)
                 (cumulative? #f)
                 (decoder (flif-create-decoder))
                 ; set the load progress to 0
                 ;(set-box! flif-load-progress 0)
                 ; progressive decoding
                 (flif-decoder-set-callback! (decoder) progressive-callback)
                 (flif-decoder-set-first-callback-quality! (decoder) 10000)
                 ; put the actual decoding in its own thread
                 #;(decoder-thread
                  (thread (λ ()
                            ; decode, but do not immediately destroy the decoder
                            (flif-decoder-decode-file! (decoder) img))))
                 ; regular decoding
                 (flif-decoder-decode-file! (decoder) img)]
                [else (send image-bmp-master load-file img 'unknown/alpha)]))
        (cond [load-success
               (send (send canvas get-parent) set-label (path->string name))
               (set! image-pict (scale-image image-bmp-master scale))
               (set! image-bmp (pict->bitmap (transparency-grid-append image-pict)))
               (define size (file-size (image-path)))
               (cond
                 [(flif? (image-path))
                  (define dimensions (flif-dimensions (image-path)))
                  (send sbd set-label
                        (format "~a x ~a pixels  ~a"
                                (first dimensions)
                                (second dimensions)
                                (cond [(>= size (expt 2 20))
                                       (format "~a MiB"
                                               (~r (exact->inexact (/ size (expt 2 20)))
                                                   #:precision 1))]
                                      [(>= size (expt 2 10))
                                       (format "~a KiB"
                                               (~r (exact->inexact (/ size (expt 2 10)))
                                                   #:precision 1))]
                                      [else
                                       (format "~a B" size)])))]
                 [else
                  (send sbd set-label
                        (format "~a x ~a pixels  ~a"
                                (send image-bmp-master get-width)
                                (send image-bmp-master get-height)
                                (cond [(>= size (expt 2 20))
                                       (format "~a MiB"
                                               (~r (exact->inexact (/ size (expt 2 20)))
                                                   #:precision 1))]
                                      [(>= size (expt 2 10))
                                       (format "~a KiB"
                                               (~r (exact->inexact (/ size (expt 2 10)))
                                                   #:precision 1))]
                                      [else
                                       (format "~a B" size)])))])
               (send sbp set-label
                     (format "~a / ~a"
                             (+ (get-index img (pfs)) 1)
                             (length (pfs))))
               (set! image-lst empty)
               (set! image-lst-timings empty)]
              [else
               (eprintf "Error loading file ~v\n" img)
               (send sbe set-label
                     (format "Error loading file ~v"
                             (string-truncate (path->string name) 30)))])])
     
     ; pick what string to display for tags...
     (cond [(db-has-key? 'images img-str)
            (define img-obj (make-data-object sqlc image% img-str))
            (define tags (send img-obj get-tags))
            (incoming-tags (string-join tags ", "))
            (when (db-has-key? 'ratings img-str)
              (define rating-obj (make-data-object sqlc rating% img-str))
              (define rating (number->string (send rating-obj get-rating)))
              (send iar set-string-selection (string-append rating " 🌟")))]
           [else (incoming-tags "")])
     ; check to see if the image has embedded tags and use them instead of
     ; what's in the DB because it may be out of date
     (cond [(embed-support? img-str)
            (set-box! image-xmp (get-embed-xmp img-str))
            (define embed-lst (get-embed-tags img-str))
            (unless (empty? embed-lst)
              ; the embedded tags may come back unsorted
              (incoming-tags (string-join (sort embed-lst string<?) ", ")))
            ; set the label of ivy-actions-rating to the rating of the
            ; image (if applicable)
            (define rating (if (empty? (unbox image-xmp))
                               "0"
                               (xmp-rating (first (unbox image-xmp)))))
            (send iar set-string-selection (string-append rating " 🌟"))]
           [else (set-box! image-xmp empty)])

     ; ...put them in the tfield
     (send tag-tfield set-value (incoming-tags))
     ; ensure the text-field displays the changes we just made
     (send tag-tfield refresh)]
    [(list? img)
     ; scale the image in the desired direction
     (set! image-lst (map (λ (pct) (scale-image pct scale)) img))]
    [else
     ; we already have the image loaded
     (set! image-lst-master empty)
     (set! image-lst empty)
     (set! image-lst-timings empty)
     (set! image-pict (scale-image img scale))
     (set! image-bmp (pict->bitmap (transparency-grid-append image-pict)))])
  
  (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
    (kill-thread (animation-thread)))
  
  (if (not (empty? image-lst))
      ; gif-lst contains a list of picts, display the animated gif
      (send canvas set-on-paint!
            (λ (canvas dc)
              (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
                (kill-thread (animation-thread)))
              
              (send dc set-background "black")
              
              (animation-thread
               (thread
                (λ ()
                  (animation-callback canvas dc image-lst))))))
      ; otherwise, display the static image
      (send canvas set-on-paint!
            (λ (canvas dc)
              (when (and (path? img) (eq? scale 'default))
                ; have the canvas re-scale the image so when the canvas is
                ; resized, it'll also be the proper size
                (set! image-pict (scale-image image-bmp-master 'default))
                (set! image-bmp (pict->bitmap (transparency-grid-append image-pict))))
              
              (define img-width (inexact->exact (round (pict-width image-pict))))
              (define img-height (inexact->exact (round (pict-height image-pict))))
              
              (define img-center-x (/ img-width 2))
              (define img-center-y (/ img-height 2))
              (define canvas-x (send canvas get-width))
              (define canvas-y (send canvas get-height))
              (define canvas-center-x (/ canvas-x 2))
              (define canvas-center-y (/ canvas-y 2))
              
              ; keep the background black
              (send canvas set-canvas-background color-black)
              
              (cond
                ; if the image is really big, place it at (0,0)
                [(and (> img-width canvas-x)
                      (> img-height canvas-y))
                 (send canvas show-scrollbars #t #t)
                 (send dc draw-bitmap image-bmp 0 0)]
                ; if the image is wider than the canvas,
                ; place it at (0,y)
                [(> img-width canvas-x)
                 (send canvas show-scrollbars #t #f)
                 (send dc draw-bitmap image-bmp
                       0 (- canvas-center-y img-center-y))]
                ; if the image is taller than the canvas,
                ; place it at (x,0)
                [(> img-height canvas-y)
                 (send canvas show-scrollbars #f #t)
                 (send dc draw-bitmap image-bmp
                       (- canvas-center-x img-center-x) 0)]
                ; otherwise, place it at the normal position
                [else
                 (send canvas show-scrollbars #f #f)
                 (send dc draw-bitmap image-bmp
                       (- canvas-center-x img-center-x)
                       (- canvas-center-y img-center-y))]))))
  
  ; tell the scrollbars to adjust for the size of the image
  (let ([img-x (inexact->exact (round (pict-width (if image-pict image-pict (first image-lst)))))]
        [img-y (inexact->exact (round (pict-height (if image-pict image-pict (first image-lst)))))])
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
       (define ratio-x (exact->inexact (/ client-center-x virtual-x)))
       (define ratio-y (exact->inexact (/ client-center-y virtual-y)))
       (send canvas init-auto-scrollbars width height
             (if (> ratio-x 1.0)
                 1.0
                 ratio-x)
             (if (> ratio-y 1.0)
                 1.0
                 ratio-y))]
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
  (unless (equal? (image-path) root-path)
    ; kill the animation thread, if applicable
    (unless (or (false? (animation-thread)) (thread-dead? (animation-thread)))
      (kill-thread (animation-thread)))
    #;(unless (or (false? (decoder-thread)) (thread-dead? (decoder-thread)))
      (kill-thread (decoder-thread))
      (displayln "load-image-in-collection; aborting decoder...")
      (flif-abort-decoder! (decoder))
      (flif-destroy-decoder! (decoder))
      (displayln "done")
      (decoder #f))
    (when (decoder)
      (flif-abort-decoder! (decoder))
      (flif-destroy-decoder! (decoder))
      (decoder #f))
    (send (ivy-tag-tfield) set-field-background color-white)
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
(define/contract (generate-thumbnails imgs)
  ((listof path-string?) . -> . void?)
  (for ([path (in-list imgs)])
    ; create and load the bitmap
    (define ext (path-get-extension path))
    (define thumb-bmp
      (cond [(bytes=? ext #".svg")
             (load-svg-from-file path)]
            [(bytes=? ext #".flif")
             (define dec (flif-create-decoder))
             (flif-decoder-decode-file! dec path)
             (parameterize ([want-animation? #f])
               (define bmp (first (flif->list dec)))
               (flif-destroy-decoder! dec)
               bmp)]
            [else
             (read-bitmap path)]))
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
          (define start (send editor get-start-position))
          (define end (send editor get-end-position))
          ; if we have something selected, only delete that part
          (send editor delete start (if (= start end)
                                        (+ end 1)
                                        end))))
  
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
