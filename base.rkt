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

(define (macosx?)
  (eq? (system-type) 'macosx))

; master dictionary
; (absolute-file-path . '(sorted list of tags))
(define master (make-hash))
(define ivy-path (cond [(eq? (system-type) 'unix)
                        ; check XDG variable first, then default
                        ; to ~/.config/ivy
                        (let ([xdg (getenv "XDG_CONFIG_HOME")])
                          (if xdg
                              (build-path xdg "ivy")
                              (build-path (find-system-path 'home-dir)
                                          ".config/ivy")))]
                       [(eq? (system-type) 'windows)
                        (normal-case-path
                         (build-path (find-system-path 'home-dir)
                                     "appdata/local/ivy"))]
                       [(macosx?)
                        (build-path (find-system-path 'home-dir)
                                    "Library/Application Support/ivy")]))
(define master-file (build-path ivy-path "catalog.json"))
; path of the currently displayed image
(define image-path (make-parameter (build-path "/")))
; master bitmap of loaded image-path
(define image-bmp-master (make-bitmap 50 50))
; pict of the currently displayed image
(define image-pict #f)
; directory containing the currently displayed image
(define image-dir (make-parameter (find-system-path 'home-dir)))
(define supported-extensions '("png" "jpg" "jpe" "jpeg" "bmp" "gif"))
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
; path for cached thumbnails
(define thumbnails-path (build-path ivy-path "thumbnails"))
; janky!
(define logo
  (if (eq? (system-type) 'unix)
      (let* ([base "share/icons/hicolor/128x128/apps/ivy-logo-128px.png"]
             [uls (build-path "/usr/local" base)]
             [us (build-path "/usr" base)])
        (cond [(file-exists? uls) uls]
              [(file-exists? us) us]
              [else (build-path "img/ivy-logo-128px.png")]))
      (build-path "img/ivy-logo-128px.png")))

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

; dct: dictionary (master)
; type: inclusive or exclusive search (or/c 'and 'or)
; items: the tags to search for (listof string?)
; returns: list of path or empty
(define (search-dict dct type taglist)
  (define search-results
    (flatten
     (for/list ([(dict-path dict-tags) (in-dict dct)])
       (define tags-searched
         ; go through each tag and search if it matches the list
         ; for that image
         (for/list ([tag taglist])
           ; list of symbol-paths and #f
           (map (λ (dict-tag) (if (string-contains-ci dict-tag tag) dict-path #f)) dict-tags)))
       ; remove any duplicate string-contains-ci matches
       ; for images that have tags containing more than
       ; one of the same phrase (e.g. images with the tags
       ; "beach" "beach towel" will appear more than once)
       (map remove-duplicates tags-searched))))
  ; filter out any false
  ; list of symbol-paths only
  (define filtered (filter symbol? search-results))
  ; searching for a single term with 'and may produce a false negative,
  ; so use 'or instead
  (cond [(or (= (length taglist) 1)
             (eq? type 'or))
         ; turn the symbols into paths and remove any duplicates
         (map symbol->path (remove-duplicates filtered))]
        [else
         ; turn the symbols into paths and keep any duplicates
         (map symbol->path (keep-duplicates filtered))]))

; dct: dictionary
; searched: list of images (listof path?)
; exclusion: list of tags (listof string?)
; returns: list of path or empty
(define (exclude-search dct searched-imgs exclusion)
  ; list of false and paths
  (define remove-imgs-messy
    (flatten
     ; loop for each image we've searched
     (for/list ([searched (in-list searched-imgs)])
       (define ex
         (flatten
          ; loop for each tag we want to exclude
          (for/list ([exclude (in-list exclusion)])
            ; go through each of the tags in the searched images for matches
            ; with tags we want to exclude
            ; list of #f and number
            (map (λ (st) (string-contains-ci st exclude)) (dict-ref dct (path->symbol searched))))))
       ; replace each instance of a number with the path of the image we want to exclude
       (map (λ (te) (if (false? te) te searched)) ex))))
  ; remove #f and duplicates
  (define remove-imgs (remove-duplicates (filter path? remove-imgs-messy)))
  ; finally remove the excluded images from the list of searched images
  (let loop ([searched searched-imgs]
             [to-remove remove-imgs])
    (if (empty? to-remove)
        searched
        (loop (remove (first to-remove) searched) (rest to-remove)))))

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

(unless (directory-exists? thumbnails-path)
  (make-directory thumbnails-path))

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
  (send tag-tfield set-field-background (make-object color% "white"))
  (cond
    ; need to load the path into a bitmap first
    [(path? img)
     (define-values (base name must-be-dir?) (split-path img))
     (image-dir base)
     (image-path img)
     (define img-sym (path->symbol img))
     ; make sure the bitmap loaded correctly
     (define load-success (send image-bmp-master load-file img))
     (cond [load-success
            (send (send canvas get-parent) set-label (path->string name))
            (send sbd set-label
                  (format "~a x ~a pixels"
                          (send image-bmp-master get-width)
                          (send image-bmp-master get-height)))
            (set! image-pict (scale-image image-bmp-master scale))
            (send sbp set-label
                  (format "~a / ~a"
                          (+ (get-index img (pfs)) 1)
                          (length (pfs))))
            ; if we've set tags for this file before...
            (cond [(hash-has-key? master img-sym)
                   (define tag
                     (string-join (hash-ref master img-sym) ", "))
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
  (unless (or (false? image-pict) (eq? (path->symbol (image-path)) '/))
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
      [(home) (load-image (first (pfs)))]
      [(end) (load-image (last (pfs)))])))

; is this complicating things? I have no idea, but we should never
; underestimate the `cool' factor
(define load-previous-image (load-image-in-collection 'previous))
(define load-next-image (load-image-in-collection 'next))
(define load-first-image (load-image-in-collection 'home))
(define load-last-image (load-image-in-collection 'end))

; takes a list lst and a width x and
; returns a list of lists of lengths no more than x
(define (grid-list lst x)
  (define len (length lst))
  (for/list ([i len]
             #:unless (>= i (/ len x)))
    (for/list ([n x]
               #:unless (> (+ (* i x) n) (sub1 len)))
      (list-ref lst (+ (* i x) n)))))

; generates 100x100 thumbnails from a list of strings paths
; e.g. (generate-thumbnails (map path->string (search-dict master 'or "beach")))
(define (generate-thumbnails imgs)
  (for ([path (in-list imgs)])
    ; create and load the bitmap
    (define bmp (read-bitmap path))
    ; cannot have slashes in the actual name
    ; fraction slash: U+2044
    (define str (string-append (string-replace path "/" "⁄") ".png"))
    (define bmp-path (build-path thumbnails-path str))
    ; use pict to scale the image to 100x100
    (define pct (bitmap bmp))
    (define bmp-small (pict->bitmap (scale-to-fit pct 100 100)))
    (define bmp-port-out (open-output-file bmp-path
                                           #:mode 'binary
                                           #:exists 'truncate/replace))
    (printf "Writing bytes to ~a~n" bmp-path)
    (write-bytes (convert bmp-small 'png-bytes) bmp-port-out)
    (close-output-port bmp-port-out)))

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
  
  (send km add-function "mark-char-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'simple))))
  
  (send km add-function "mark-char"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'simple))))
  
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
  (send km map-function ":home" "beginning-of-buffer")
  (send km map-function ":end" "end-of-buffer")
  (send km map-function ":rightbuttonseq" "mouse-popup-menu")
  (send km map-function ":middlebutton" "insert-primary")
  (send km map-function ":s:insert" "insert-primary"))

(current-text-keymap-initializer
 (λ (keymap)
   (define km (init-editor-keymap keymap))
   (set-default-editor-bindings km)))
