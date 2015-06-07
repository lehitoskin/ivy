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
         file/convertible)
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
; parameter listof path
(define pfs (make-parameter (list (build-path "/"))))
; path for cached icons
(define icons-path (build-path ivy-path "icons"))

(define (save-dict! dct)
  (with-output-to-file master-file
    (λ () (write-json dct))
    #:exists 'truncate/replace
    #:mode 'text))

(define (clear-dict! dct)
  (for ([file-sym (in-dict-keys dct)])
    (define file-path (symbol->path file-sym))
    (printf "Checking ~s...~n" file-path)
    (unless (file-exists? file-path)
      (printf "Removing ~s from dictionary.~n" file-path)
      (dict-remove! dct file-sym))))

; enter a dictionary (master) and tag strings to search for.
; returns a list of image paths or empty on failure
;
; TODO:
; search dictionary and only return images that fit ALL
;   the tags given
; make it an option to do specific search or general search?
;
; default is 'or search (this tag OR that tag)
;
; need the whole tag list for that image instead of the list
; returned from member to compare the next item in the search
; results
;
; what is really needed is the opposite of remove-duplicates,
; something like keep-duplicates... look at andmap/ormap
(define (test-search lst)
  (define search-results
    (for/list ([(paths tags) (in-dict master)])
      (define result (if (member (car lst) tags) tags #f))
      (for/list ([l (cdr lst)])
        (cond [result (if (member l result)
                          tags
                          #f)]
              [else #f]))
      ; take the tags and turn them into symbol-paths
      (if result
          (map (λ (j) (if (false? j) j paths)) result)
          #f)))
  (define filtered (sort (filter symbol? (flatten search-results)) symbol<?))
  (remove-duplicates (map symbol->path filtered)))

#|
(let ([or-results
         (flatten
          (for/list ([(path tag) (in-dict master)])
            (define results
              (flatten
               (for/list ([item '("Tsutomu Nihei" "beach")])
                 (define res (if (member item tag) tag #f))
                 (if res
                     (map (λ (l) (if (false? l) l path)) res)
                     #f))))
            (filter symbol? results)))])
    (define xor
      (remove-duplicates or-results))
    xor)
|#

(define (search-dict dct [type 'or] . items)
  (case type
    [(or)
     (define search-results
       (flatten
        (for/list ([(path tag) (in-dict dct)])
          ; list of tags and #f
          (define result (map (λ (i) (member i tag)) items))
          ; check for false through the result list
          ; if not false, return the path for the result
          ; list of symbol-paths and #f
          (map (λ (l) (if (false? l) l path)) result))))
     ; filter out any false
     ; list of symbol-paths only
     (define filtered (filter symbol? search-results))
     ; turn the symbols into paths and remove any duplicates
     (remove-duplicates (map symbol->path filtered))]
    [(xor)
     (define or-results
       (for/list ([(path tag) (in-dict master)])
         (define results
           (flatten
            (for/list ([item items])
              (define res (if (member item tag) tag #f))
              (if res
                  (map (λ (l) (if (false? l) l path)) res)
                  #f))))
         (filter symbol? results)))
     (remove-duplicates (flatten or-results))]))
    #;[(and)
     (define search-results
       (flatten
        (for/list ([(paths tags) (in-dict dct)])
          (define result (map (λ (i) (member i tags)) items)))))]

; create the config directory
(unless (directory-exists? ivy-path)
  (make-directory ivy-path))

; load the dictionary file
; this could get very big!
(when (file-exists? master-file)
  (define json-port (open-input-file master-file))
  (define dict-json (read-json json-port))
  (for ([(k v) (in-dict dict-json)])
    (dict-set! master k v))
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
(define (scale-image canvas img type)
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

; procedure that loads the given image to the canvas
(define (load-image img [scale 'default])
  (define canvas (ivy-canvas))
  (define tag-tfield (ivy-tag-tfield))
  (define sbd (status-bar-dimensions))
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (set! image-dir base)
     (set! image-path (path->symbol img))
     ; make sure the bitmap loaded correctly
     (define load-success (send image-bmp-master load-file img))
     (cond [load-success
            (send (send canvas get-parent) set-label (path->string name))
            (send sbd set-label
                  (format "~a x ~a pixels"
                          (send image-bmp-master get-width)
                          (send image-bmp-master get-height)))
            (set! image-pict (scale-image canvas image-bmp-master scale))
            ; if we've set tags for this file before...
            (cond [(hash-has-key? master image-path)
                   (define tag
                     (string-join (hash-ref master image-path) ","))
                   ; ...put them in the tfield
                   (send tag-tfield set-value tag)]
                  ; ...otherwise clear the tfield
                  [else (send tag-tfield set-value "")])]
           [else (printf "Error loading file ~a~n" img)])]
    [else
     ; we already have the image loaded
     (set! image-pict (scale-image canvas img scale))])
  
  (define bmp (pict->bitmap image-pict))
  (define width (send bmp get-width))
  (define height (send bmp get-height))
  
  (send canvas set-on-paint!
        (λ ()
          (define dc (send canvas get-dc))
          
          (define bmp-center-x (/ width 2))
          (define bmp-center-y (/ height 2))
          (define canvas-x (send canvas get-width))
          (define canvas-y (send canvas get-height))
          (define canvas-center-x (/ canvas-x 2))
          (define canvas-center-y (/ canvas-y 2))
          
          ; keep the background black
          (send canvas set-canvas-background
                (make-object color% "black"))
          
          (cond
            ; if the image is really big, place it at (0,0)
            [(and (> width canvas-x)
                  (> height canvas-y))
             (send canvas show-scrollbars #t #t)
             (send dc draw-bitmap bmp 0 0)]
            ; if the image is wider than the canvas,
            ; place it at (0,y)
            [(> width canvas-x)
             (send canvas show-scrollbars #t #f)
             (send dc draw-bitmap bmp
                   0 (- canvas-center-y bmp-center-y))]
            ; if the image is taller than the canvas,
            ; place it at (x,0)
            [(> height canvas-y)
             (send canvas show-scrollbars #f #t)
             (send dc draw-bitmap bmp
                   (- canvas-center-x bmp-center-x) 0)]
            ; otherwise, place it at the normal position
            [else
             (send canvas show-scrollbars #f #f)
             (send dc draw-bitmap bmp
                   (- canvas-center-x bmp-center-x)
                   (- canvas-center-y bmp-center-y))])))
  
  (send canvas init-auto-scrollbars width height 0.0 0.0)
  (send canvas refresh))

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
; e.g. (generate-icons (map path->string (search-dict master "beach")))
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
