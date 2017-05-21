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
                         [width 700]
                         [height 100]))

(unless macosx?
  (void (send stats-frame set-icon logo-bmp)))

; total images
(define total-images-hpanel
  (new horizontal-panel%
       [parent stats-frame]))

(define total-images-text
  (new horizontal-panel%
       [parent total-images-hpanel]
       [alignment '(left center)]))

(define total-images-value
  (new horizontal-panel%
       [parent total-images-hpanel]
       [alignment '(right center)]))

; total tags
(define total-tags-hpanel
  (new horizontal-panel%
       [parent stats-frame]))

(define total-tags-text
  (new horizontal-panel%
       [parent total-tags-hpanel]
       [alignment '(left center)]))

(define total-tags-value
  (new horizontal-panel%
       [parent total-tags-hpanel]
       [alignment '(right center)]))

; tags per image
(define tags-per-img-hpanel
  (new horizontal-panel%
       [parent stats-frame]))

(define tags-per-img-text
  (new horizontal-panel%
       [parent tags-per-img-hpanel]
       [alignment '(left center)]))

(define tags-per-img-value
  (new horizontal-panel%
       [parent tags-per-img-hpanel]
       [alignment '(right center)]))

; vertical-panel for the rest
(define stats-vpanel
  (new vertical-panel%
       [parent stats-frame]
       [alignment '(left center)]))

; obtain the largest ocurrence `num' of tag `name'
; return values of num and name
(define (greater lst [num 0] [name ""])
  (cond [(empty? lst) (values num name)]
        [else
         (define len (length (second (first lst))))
         (if (> len num)
             (greater (rest lst) len (first (first lst)))
             (greater (rest lst) num name))]))

; give an up-to-date reading of the database each time
(define (create-children)
  (define imgs-pairs (table-pairs 'images))
  (define tags-pairs (table-pairs 'tags))

  ; total images
  (new message%
       [parent total-images-text]
       [label "Total images:"])

  (new message%
       [parent total-images-value]
       [label (format "~a" (length imgs-pairs))])

  ; total tags
  (new message%
       [parent total-tags-text]
       [label "Total tags:"])

  (new message%
       [parent total-tags-value]
       [label (format "~a" (length tags-pairs))])

  ; tags per image
  (let ([avg (/ (for/sum ([ip (in-list imgs-pairs)])
                  (length (second ip)))
                (length imgs-pairs))])
    (new message%
         [parent tags-per-img-text]
         [label "Average tags per image:"])
    (new message%
         [parent tags-per-img-value]
         [label (format "~a" (~r (exact->inexact avg) #:precision 3))]))
  
  (new message%
       [parent stats-vpanel]
       [label
        (let-values ([(num name) (greater imgs-pairs)])
          ; make sure formatted string does not exceed label-string? max-length
          (define truncation (- +label-max+ 39 (string-length (number->string num))))
          (format "Largest number of tags on an image: ~a (~a)"
                  num (string-truncate (path->string name) truncation)))])
  
  (new message%
       [parent stats-vpanel]
       [label
        (let-values ([(num name) (greater tags-pairs)])
          (define truncation (- +label-max+ 47 (string-length (number->string num))))
          (format "Largest number of images in a tag category: ~a (~a)"
                  num (string-truncate name truncation)))])
  
  (void))

(define (update-stats)
  (remove-children total-images-text (send total-images-text get-children))
  (remove-children total-images-value (send total-images-value get-children))
  (remove-children total-tags-text (send total-tags-text get-children))
  (remove-children total-tags-value (send total-tags-value get-children))
  (remove-children tags-per-img-text (send tags-per-img-text get-children))
  (remove-children tags-per-img-value (send tags-per-img-value get-children))
  (remove-children stats-vpanel (send stats-vpanel get-children))
  (create-children))
