#lang racket
;
;
(require (only-in plot/utils
                  clamp-real
                  ivl)
         (only-in racket/gui/base
                  canvas%)
         (only-in "base.rkt"
                  +root-path+
                  color-black
                  get-index
                  image-bmp-master
                  image-dir
                  image-path
                  load-first-image
                  load-image
                  load-last-image
                  load-next-image
                  load-previous-image
                  macosx?
                  pfs
                  supported-file?))

(provide (all-defined-out))


(define ivy-canvas%
  (class canvas%
    (super-new)
    
    (init-field focus-tag-tfield
                insert-tag-tfield-comma
                paint-callback
                set-fullscreen
                status-bar-position
                status-bar-zoom
                toggle-fullscreen
                [canvas-backgorund color-black])
    
    (define mouse-x 0)
    (define mouse-y 0)

    ; whether the last zoom operation was "fit"
    (define fit #f)

    (define/public (get-mouse-pos)
      (values mouse-x mouse-y))
    
    (define (do-on-paint)
      (when paint-callback
        (paint-callback this (send this get-dc))))
    
    (define/override (on-paint)
      (do-on-paint))
    
    ; proc: ((is-a?/c canvas%) (is-a?/c dc<%>) . -> . any)
    (define/public (set-on-paint! proc)
      (set! paint-callback proc))
    
    (define/override (on-drop-file pathname)
      ; append the image to the current collection
      (define-values (base name must-be-dir?) (split-path pathname))
      (define directory? (directory-exists? pathname))
      (cond
        ; empty collection
        [(equal? (first (pfs)) +root-path+)
         (cond [directory?
                (define files
                  (for/fold ([lst empty])
                            ([p (in-directory pathname)])
                    (if (supported-file? p)
                        (append lst (list p))
                        lst)))
                (image-dir pathname)
                (pfs files)
                (image-path (first files))
                (load-image (first files))]
               [else
                (image-dir base)
                (pfs (list pathname))
                (image-path pathname)
                (load-image pathname)])]
        ; collection has images; appending to collection
        [else
         (define files
           (if (directory-exists? pathname)
               (for/fold ([lst empty])
                         ([p (in-directory pathname)])
                 (if (supported-file? p)
                     (append lst (list p))
                     lst))
               (list pathname)))
         ; no duplicate paths allowed!
         (pfs (remove-duplicates (append (pfs) files)))
         ; change label because it usually isn't called until
         ; (load-image) is called and we want to see the changes now
         (send (status-bar-position) set-label
               (format "~a / ~a"
                       (+ (get-index (image-path) (pfs)) 1)
                       (length (pfs))))]))
    
    (define/override (on-event evt)
      (define type (send evt get-event-type))
      (case type
        ; track where the mouse is
        [(enter motion)
         (set! mouse-x (send evt get-x))
         (set! mouse-y (send evt get-y))]))
    
    (define/override (on-char key)
      (define type (send key get-key-code))
      (case type
        [(wheel-down)
         ; do nothing if we've pressed ctrl+n
         (unless (equal? (image-path) +root-path+)
           (send this zoom-by -0.05))]
        [(wheel-up)
         ; do nothing if we've pressed ctrl+n
         (unless (equal? (image-path) +root-path+)
           (send this zoom-by 0.05))]
        ; osx does things a little different
        [(f11) (unless macosx?
                 (toggle-fullscreen this))]
        ; only do something if we're fullscreened,
        ; since the tag bar isn't available in fullscreen anyway
        [(escape) (when (not macosx?)
                    (set-fullscreen #f))]
        [(left) (load-previous-image)]
        [(right) (load-next-image)]
        [(home) (load-first-image)]
        [(end) (load-last-image)]
        [(#\,) (focus-tag-tfield)
               (insert-tag-tfield-comma)]
        [(#\return) (focus-tag-tfield)]))

    (define/private (configure-scrollbars zoom-factor)
      (let* ([img-w (send image-bmp-master get-width)]
             [img-h (send image-bmp-master get-height)]
             [zoomed-img-w (inexact->exact (round (* img-w zoom-factor)))]
             [zoomed-img-h (inexact->exact (round (* img-h zoom-factor)))]
             [client-w (send this get-width)]
             [client-h (send this get-height)]
             [virtual-w (max client-w zoomed-img-w)]
             [virtual-h (max client-h zoomed-img-h)]
             [scroll-x 0.5] ; TODO
             [scroll-y 0.5]) ; TODO
        (send this init-auto-scrollbars virtual-w virtual-h scroll-x scroll-y)
        (send this show-scrollbars
              (> zoomed-img-w client-w)
              (> zoomed-img-h client-h))))

    ; zooms to a specific zoom-factor (1.0 == "no zoom"),
    ; with optional staus bar label override
    (define/public (zoom-to factor [status-label #f])
      (set! fit #f) ; always make sure this is cleared when setting a new zoom level
      (define dc (send this get-dc))
      (send dc set-scale factor factor)
      (configure-scrollbars factor)
      (send this refresh-now)
      (send (status-bar-zoom) set-label
            (cond [status-label status-label]
                  [(not (= factor 1.0)) (format "@ ~aX" (~r factor #:precision 2))]
                  [else ""])))

    ; zooms view by a specified increment (positive or negative)
    (define/public (zoom-by inc)
      (define dc (send this get-dc))
      (define-values [cur-scale-x cur-scale-y]
        (send dc get-scale))
      (define new-scale
        (clamp-real (+ cur-scale-x inc) (ivl 0.1 4.0)))
      (send this zoom-to new-scale))

    ; adjusts zoom level so the entire image fits, and at least one dimension
    ; will be the same size as the window.
    (define/public (zoom-to-fit)
      (let* ([client-w (send this get-width)]
             [client-h (send this get-height)]
             [img-w (send image-bmp-master get-width)]
             [img-h (send image-bmp-master get-height)]
             [new-zoom (min (/ client-w img-w)
                            (/ client-h img-h))])
        (send this zoom-to new-zoom "[Fit]")
        ; must set this *after* calling zoom-to, where it is reset to false
        (set! fit #t)))

    ; only zooms out if the image is too big to fit on either dimension
    (define/public (center-fit)
      (let ([client-w (send this get-width)]
            [client-h (send this get-height)]
            [img-w (send image-bmp-master get-width)]
            [img-h (send image-bmp-master get-height)])
        (cond [(or (> img-w client-w)
                   (> img-h client-h))
               (send this zoom-to-fit)]
              [else (send this zoom-to 1.0)])))

    (define/override (on-size width height)
      (recenter-origin width height)
      (if fit
          (send this zoom-to-fit)
          (send this refresh-now)))

    (define/private (recenter-origin width height)
      (define dc (send this get-dc))
      (send dc set-origin
        (/ width 2)
        (/ height 2)))

    (define/public (recenter)
      (define-values [virtual-w virtual-h] (send this get-virtual-size))
      (recenter-origin virtual-w virtual-h))))
