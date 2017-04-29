#lang racket/base
; files.rkt
; definitions of file paths that ivy uses
(require images/compile-time
         (for-syntax racket/base racket/draw)
         racket/file)
(provide (all-defined-out))

(define ivy-version "2.2.2")

; base directory where ivy will put all of its files
(define ivy-path
  (case (system-type)
    [(unix)
     ; check XDG variable first, then default to ~/.config/ivy
     (let ([xdg (getenv "XDG_CONFIG_HOME")])
       (if xdg
           (build-path xdg "ivy")
           (build-path (find-system-path 'home-dir)
                       ".config/ivy")))]
    [(windows)
     (normal-case-path
      (build-path (find-system-path 'home-dir)
                  "appdata/local/ivy"))]
    [(macosx)
     (build-path (find-system-path 'home-dir)
                 "Library/Application Support/ivy")]))

(define master-file (build-path ivy-path "catalog.sqlite"))

; path for cached thumbnails
; - on *NIX, use ~/.cache/thumbnails/normal
(define thumbnails-path
  (if (eq? (system-type) 'unix)
      (let ([xdg (getenv "XDG_CACHE_HOME")])
        (if xdg
            (build-path xdg "thumbnails/normal")
            (build-path (find-system-path 'home-dir)
                        ".cache/thumbnails/normal")))
      (build-path ivy-path "thumbnails")))

(begin-for-syntax
  (define logo
    (if (eq? (system-type) 'unix)
        (let* ([base "share/icons/hicolor/128x128/apps/ivy-logo-128px.png"]
               [uls (build-path "/usr/local" base)]
               [us (build-path "/usr" base)])
          (cond [(file-exists? uls) uls]
                [(file-exists? us) us]
                [else (build-path "img/ivy-logo-128px.png")]))
        (build-path "img/ivy-logo-128px.png"))))

(define logo-bmp (compiled-bitmap (read-bitmap logo)))

; create the config directory
(unless (directory-exists? ivy-path)
  (make-directory* ivy-path))

(unless (directory-exists? thumbnails-path)
  (make-directory* thumbnails-path)
  (file-or-directory-permissions thumbnails-path #o700))

; flif decoder parameter
(define decoder (make-parameter #f))
