#lang racket/base
; main.rkt
; main file for ivy, the taggable image viewer
(require racket/class
         "base.rkt"
         "frame.rkt")

(send ivy-frame show #t)
