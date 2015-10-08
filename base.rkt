#lang racket/base
; base.rkt
; base file for ivy, the taggable image viewer
(require json
         pict
         racket/gui/base
         racket/dict
         racket/bool
         racket/list
         racket/class
         racket/string
         racket/path
         file/convertible
         (only-in srfi/13
                  string-contains-ci))
(provide (all-defined-out))

(define (path->symbol p)
  (string->symbol (path->string p)))

(define (symbol->path p)
  (string->path (symbol->string p)))

; master dictionary
; (absolute-file-path . '(sorted list of tags))
(define master (make-hash))
(define ivy-path (cond [(eq? (system-type) 'unix)
                        (build-path (find-system-path 'home-dir)
                                    ".config/ivy")]
                       [(eq? (system-type) 'windows)
                        (normal-case-path
                         (build-path (find-system-path 'home-dir)
                                     "appdata/local/ivy"))]
                       [(eq? (system-type) 'macosx)
                        (build-path (find-system-path 'home-dir)
                                    "Library/Application Support/ivy")]))
(define master-file (build-path ivy-path "catalog.json"))
; path of the currently displayed image
(define image-path (make-parameter '/))
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; pict of the currently displayed image
(define image-pict #f)
; directory containing the currently displayed image
(define image-dir (make-parameter (find-system-path 'home-dir)))
(define supported-extensions '("png" "jpg" "jpeg" "bmp" "gif"))
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
(define pfs (make-parameter (list (build-path "/"))))
; path for cached icons
(define icons-path (build-path ivy-path "icons"))

(define (save-dict! dct)
  (with-output-to-file master-file
    (λ () (write-json dct))
    #:exists 'truncate/replace
    #:mode 'text))

; removes entries for files that no longer exist
(define (clean-dict! dct)
  (define old-dct (dict-copy dct))
  (for ([sym (in-dict-keys old-dct)])
    (define path (symbol->path sym))
    (unless (file-exists? path)
      (printf "Removing ~s from dictionary.~n" path)
      (dict-remove! dct sym)))
  (save-dict! dct))

; saves only the entries in the list that are duplicates.
; if there are more than two identical entries, they are
; counted more than once, so a final sort and remove-duplicates
; (how ironic) is possibly necessary.
(define (keep-duplicates lst [dups empty])
  (define sorted (sort lst equal?))
  (define len (length sorted))
  (cond [(< len 2) (remove-duplicates dups)]
        [(>= len 2)
         (if (equal? (first sorted) (second sorted))
             (keep-duplicates (rest sorted) (cons (first sorted) dups))
             (keep-duplicates (rest sorted) dups))]))

; enter a dictionary (master) and tag strings to search for.
; returns a list of image paths or empty on failure
(define (search-dict dct type . items)
  (define search-results
    (flatten
     (for/list ([(path tags) (in-dict dct)])
       (define ts
         ; go through each tag and search if it matches the list
         ; for that image
         (for/list ([i items])
           ; list of tags and #f
           ;(define result (map (λ (i) (member i tags)) items))
           ; list of numbers and #f
           (define result (map (λ (t) (string-contains-ci t i)) tags))
           ; check for false through the result list
           ; if not false, return the path for the result
           ; list of symbol-paths and #f
           (map (λ (l) (if (false? l) l path)) result)))
       ; remove any duplicate string-contains-ci matches
       ; for images that have tags containing more than
       ; one of the same phrase (e.g. images with the tags
       ; "beach" "beach towel" will appear more than once)
       (map remove-duplicates ts))))
  ; filter out any false
  ; list of symbol-paths only
  (define filtered (filter symbol? search-results))
  (case type
    [(or)
     ; turn the symbols into paths and remove any duplicates
     (map symbol->path (remove-duplicates filtered))]
    [(and)
     ; turn the symbols into paths and keep any duplicates
     (map symbol->path (keep-duplicates filtered))]))

; create the config directory
(unless (directory-exists? ivy-path)
  (make-directory ivy-path))

; load the dictionary file
; this could get very big!
(when (file-exists? master-file)
  (define json-port (open-input-file master-file))
  ; Racket v6.2.1 read-json returns immutable hash.
  ; we need to operate with a mutable one
  (set! master (hash-copy (read-json json-port)))
  (close-input-port json-port))

(unless (directory-exists? icons-path)
  (make-directory icons-path))

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
     (set! max-height 501)
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

; janky!
(define ivy-canvas (make-parameter #f))
(define ivy-tag-tfield (make-parameter #f))
(define status-bar-dimensions (make-parameter #f))
(define status-bar-position (make-parameter #f))

; procedure that loads the given image to the canvas
; takes care of updating the dimensions message and
; the position message
(define (load-image img [scale 'default])
  (define canvas (ivy-canvas))
  (define tag-tfield (ivy-tag-tfield))
  (define sbd (status-bar-dimensions))
  (define sbp (status-bar-position))
  (send tag-tfield set-label "Edit tag(s) : ")
  (send tag-tfield set-field-background (make-object color% "white"))
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (image-dir base)
     (image-path (path->symbol img))
     ; make sure the bitmap loaded correctly
     (define load-success (send image-bmp-master load-file img))
     (cond [load-success
            (send (send canvas get-parent) set-label (path->string name))
            (send sbd set-label
                  (format "~a x ~a pixels"
                          (send image-bmp-master get-width)
                          (send image-bmp-master get-height)))
            (set! image-pict (scale-image image-bmp-master scale))
            (send (status-bar-position) set-label
                  (format "~a / ~a"
                          (+ (get-index img (pfs)) 1)
                          (length (pfs))))
            ; if we've set tags for this file before...
            (cond [(hash-has-key? master (image-path))
                   (define tag
                     (string-join (hash-ref master (image-path)) ", "))
                   ; ...put them in the tfield
                   (send tag-tfield set-value tag)]
                  ; ...otherwise clear the tfield
                  [else (send tag-tfield set-value "")])]
           [else (printf "Error loading file ~a~n" img)])]
    [else
     ; we already have the image loaded
     (set! image-pict (scale-image img scale))])
  
  (send canvas set-on-paint!
        (λ ()
          (define dc (send canvas get-dc))
          
          (when (or (path? img) (eq? scale 'default))
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
          
          (cond
            ; if the image is really big, place it at (0,0)
            [(and (> img-width canvas-x)
                  (> img-height canvas-y))
             (send canvas show-scrollbars #t #t)
             (draw-pict image-pict dc 0 0)]
            ; if the image is wider than the canvas,
            ; place it at (0,y)
            [(> img-width canvas-x)
             (send canvas show-scrollbars #t #f)
             (draw-pict image-pict dc
                        0 (- canvas-center-y img-center-y))]
            ; if the image is taller than the canvas,
            ; place it at (x,0)
            [(> img-height canvas-y)
             (send canvas show-scrollbars #f #t)
             (draw-pict image-pict dc
                        (- canvas-center-x img-center-x) 0)]
            ; otherwise, place it at the normal position
            [else
             (send canvas show-scrollbars #f #f)
             (draw-pict image-pict dc
                        (- canvas-center-x img-center-x)
                        (- canvas-center-y img-center-y))])))
  
  (let* ([width (inexact->exact (round (pict-width image-pict)))]
         [height (inexact->exact (round (pict-height image-pict)))])
    (send canvas init-auto-scrollbars width height 0.0 0.0))
  (send canvas refresh))

; curried procedure to abstract loading an image in a collection
; mmm... curry
(define ((load-image-in-collection direction))
  (unless (or (false? image-pict) (eq? (image-path) '/))
    (define prev-index (get-index (symbol->path (image-path)) (pfs)))
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
                (load-image img)]))])))

; is this complicating things? I have no idea, but we should never
; underestimate the `cool' factor
(define load-previous-image (load-image-in-collection 'previous))
(define load-next-image (load-image-in-collection 'next))

; takes a list lst and a width x and
; returns a list of lists of lengths no more than x
(define (grid-list lst x)
  (define len (length lst))
  (for/list ([i len]
             #:unless (>= i (/ len x)))
    (for/list ([n x]
               #:unless (> (+ (* i x) n) (sub1 len)))
      (list-ref lst (+ (* i x) n)))))

; generates 100x100 icons from a list of strings paths
; e.g. (generate-icons (map path->string (search-dict master 'or "beach")))
(define (generate-icons imgs)
  (for ([path (in-list imgs)])
    ; create and load the bitmap
    (define bmp (make-bitmap 100 100))
    (send bmp load-file path)
    ; cannot have slashes in the actual name
    ; fraction slash: U+2044
    (define str (string-append (string-replace path "/" "⁄") ".png"))
    (define bmp-path (build-path icons-path str))
    ; use pict to scale the image to 100x100
    (define pct (bitmap bmp))
    (define bmp-small (pict->bitmap (scale-to-fit pct 100 100)))
    (define bmp-port-out (open-output-file bmp-path
                                           #:mode 'binary
                                           #:exists 'truncate/replace))
    (printf "Writing bytes to ~a~n" bmp-path)
    (write-bytes (convert bmp-small 'png-bytes) bmp-port-out)
    (close-output-port bmp-port-out)))
