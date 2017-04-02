#lang racket/base
; thumbnails.rkt
(require file/convertible
         file/md5
         net/uri-codec
         pict
         png-image
         racket/contract
         racket/draw
         racket/list
         racket/path
         racket/string
         riff
         rsvg
         "base.rkt"
         "files.rkt")
(provide path->md5 generate-thumbnails)

(define/contract (path->uri path)
  (path-string? . -> . string?)
  ; convert the path-string to a valid URI
  (define path-lst (explode-path path))
  ; the first entry is going to be garbled, so omit that part
  (define encoded (map (compose1 uri-encode path->string) (rest path-lst)))
  (string-append "file://"
                 (if (eq? (system-type) 'windows)
                     "/c:/"
                     "/")
                 (string-join encoded "/")))

; returns the absolute path to the thumbnail we want,
; with the name of the thumbnail being an md5sum
; md5 *of the path*, not the image
(define/contract (path->md5 path)
  (path-string? . -> . path?)
  (define uri (path->uri path))
  ; encode to md5
  (define thumb-name (bytes->string/utf-8 (bytes-append (md5 uri #t) #".png")))
  (build-path thumbnails-path thumb-name))

; generates 128x128 thumbnails from a list of string paths
; e.g. (generate-thumbnails (map path->string (search-dict master 'or "beach")))
(define/contract (generate-thumbnails imgs)
  ((listof path-string?) . -> . void?)
  (for ([path (in-list imgs)])
    ; create and load the bitmap
    (define ext (path-get-extension path))
    (define thumb-bmp
      (case ext
        [(#".svg" #".SVG")
         (load-svg-from-file path)]
        [(#".flif" #".FLIF" #".flaf" #".FLAF")
         (define dec (flif-create-decoder))
         (flif-decoder-decode-file! dec path)
         (parameterize ([want-animation? #f])
           (define bmp (first (flif->list dec)))
           (flif-destroy-decoder! dec)
           bmp)]
        [else (read-bitmap path)]))
    (define thumb-path (path->md5 path))
    ; use a temporary file in case there's concurrent
    ; thumbnail generation going on
    (define thumb-tmp (string-append (number->string (current-seconds)) "_ivy-tmp.png"))
    ; use pict to scale the image to 128x128
    (define thumb-pct (bitmap thumb-bmp))
    (define thumb-small (pict->bitmap (scale-to-fit thumb-pct 128 128)))
    (define thumb-port-out (open-output-file thumb-tmp
                                             #:mode 'binary
                                             #:exists 'truncate/replace))
    (printf "Writing bytes to ~a~n" thumb-path)
    (define thumb-hash (png->hash (convert thumb-small 'png-bytes)))
    ; set thumbnail attributes
    (define uri (path->uri path))
    (define mtime (file-or-directory-modify-seconds path))
    (define mime
      (case ext
        [(#".svg" #".SVG") "image/svg+xml"]
        [(#".flif" #".FLIF" #".flaf" #".FLAF") "image/flif"]
        [(#".jpg" #".JPG" #".jpeg" #".JPEG" #".jpe" #".JPE") "image/jpeg"]
        [(#".png" #".PNG") "image/png"]
        [(#".gif" #".GIF") "image/gif"]
        [(#".bmp" #".BMP") "image/bmp"]
        [(#".xbm" #".XBM") "image/x-xbitmap"]
        [(#".xpm" #".XPM") "image/x-xpixmap"]))
    (define size (file-size path))
    (define software (format "Ivy Image Viewer ~a" ivy-version))
    (define setted
      (let* ([mti (text-set thumb-hash
                            (make-text-hash
                             (make-text-chunk (number->string mtime) "Thumb::MTime"))
                            "Thumb::MTime")]
             [ur (text-set mti
                           (make-text-hash
                            (make-text-chunk uri "Thumb::URI"))
                           "Thumb::URI")]
             [sz (text-set ur
                           (make-text-hash
                            (make-text-chunk (number->string size) "Thumb::Size"))
                           "Thumb::Size")]
             [mty (text-set sz
                            (make-text-hash
                             (make-text-chunk mime "Thumb::Mimetype"))
                            "Thumb::Mimetype")])
        (text-set mty (make-text-hash (make-text-chunk software "Software")) "Software")))
    ; save to disk
    (write-bytes (hash->png setted) thumb-port-out)
    (close-output-port thumb-port-out)
    ; rename thumb-tmp to thumb-path
    (rename-file-or-directory thumb-tmp thumb-path #t)
    (file-or-directory-permissions thumb-path #o600)))
