#lang racket/base
; db-statistics.rkt
(require racket/class
         racket/format
         racket/gui/base
         racket/list
         "base.rkt"
         "db.rkt"
         "files.rkt")
(provide stats-frame update-stats)

(define stats-frame (new frame%
                         [label "Ivy Statistics"]
                         [width 800]
                         [height 100]))

(unless macosx?
  (void (send stats-frame set-icon logo-bmp)))

(define stats-vpanel
  (new vertical-panel%
       [parent stats-frame]
       [alignment '(left center)]))

(define (greater lst [num 0] [name ""])
  (cond [(empty? lst) (values num name)]
        [else
         (define len (length (second (first lst))))
         (if (> len num)
             (greater (rest lst) len (first (first lst)))
             (greater (rest lst) num name))]))

; give an up-to-date reading of the database
(define (create-children)
  (define imgs-pairs (table-pairs 'images))
  (define tags-pairs (table-pairs 'tags))
  
  (new message%
       [parent stats-vpanel]
       [label (format "Total images/tags: ~a / ~a" (length imgs-pairs) (length tags-pairs))])
  
  (let ([avg (/ (for/sum ([ip (in-list imgs-pairs)])
                  (length (second ip)))
                (length imgs-pairs))])
    (new message%
         [parent stats-vpanel]
         [label (format "Average tags per image: ~a" (~r (exact->inexact avg) #:precision 3))]))
  
  (new message%
       [parent stats-vpanel]
       [label
        (let-values ([(num name) (greater imgs-pairs)])
          (format "Largest number of tags on an image: ~a (~a)"
                  num name))])
  
  (new message%
       [parent stats-vpanel]
       [label
        (let-values ([(num name) (greater tags-pairs)])
          (format "Largest number of images in a tag category: ~a (~a)"
                  num name))])
  
  (void))

(define (update-stats)
  (remove-children stats-vpanel (send stats-vpanel get-children))
  (create-children))
