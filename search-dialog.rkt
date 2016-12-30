#lang racket/base
; search-dialog.rkt
(require racket/class
         racket/gui/base
         racket/list
         "base.rkt"
         "db.rkt"
         "search-results.rkt")
(provide search-tag-dialog
         search-tfield)

(define (ok-callback)
  (send search-tag-dialog show #f)
  (define tags (tfield->list search-tfield))
  (define search-type
    (string->symbol
     (send type-rbox get-item-label
           (send type-rbox get-selection))))
  ; make sure there aren't any nonexistant files in the dictionary
  (clean-db!)
  (define imgs
    (if (empty? tags)
        ; table-column: (or/c (listof path?) empty?)
        (table-column 'images 'Path)
        (if (exact-search?)
            (search-db-exact search-type tags)
            (search-db-inexact search-type tags))))
  (define exclude-tags (tfield->list exclude-tfield))
  (cond [(empty? imgs)
         (display-nil-results-alert)
         (send (send search-tfield get-editor) select-all)
         (send search-tag-dialog show #t)]
        [else
         (if (empty? exclude-tags)
             (display-tags imgs)
             (if (exact-search?)
                 (display-tags (exclude-search-exact imgs exclude-tags))
                 (display-tags (exclude-search-inexact imgs exclude-tags))))]))

(define search-tag-dialog
  (new dialog%
       [label "Ivy - Search Tags"]
       [width 400]
       [height 100]
       [style '(close-button)]))

(define search-tfield
  (new text-field%
       [parent search-tag-dialog]
       [label "Search tags:   "]
       [callback
        (λ (tf evt)
          (when (and
                 (eq? (send evt get-event-type) 'text-field-enter)
                 (not (string-null? (send tf get-value))))
            (ok-callback)))]))

(define exclude-tfield
  (new text-field%
       [parent search-tag-dialog]
       [label "Exclude tags: "]
       [callback
        (λ (tf evt)
          (when (and
                 (eq? (send evt get-event-type) 'text-field-enter)
                 (not (string-null? (send tf get-value))))
            (ok-callback)))]))

(define modifier-hpanel
  (new horizontal-panel%
       [parent search-tag-dialog]
       [alignment '(center center)]
       [stretchable-height #f]))

(define checkbox-pane
  (new pane%
       [parent modifier-hpanel]
       [alignment '(right center)]))

(define exact-checkbox
  (new check-box%
       [parent checkbox-pane]
       [label "Exact"]
       [value #f]
       [callback (λ (button event)
                   (exact-search? (send button get-value)))]))

(define type-pane
  (new pane%
       [parent modifier-hpanel]
       [alignment '(center center)]))

(define type-rbox
  (new radio-box%
       [parent type-pane]
       [label "Search type"]
       [choices '("and" "or")]
       [style '(horizontal)]))

(define button-hpanel
  (new horizontal-panel%
       [parent search-tag-dialog]
       [alignment '(right center)]
       [stretchable-height #f]))

(define cancel-button
  (new button%
       [parent button-hpanel]
       [label "&Cancel"]
       [callback (λ (button event)
                   (send search-tag-dialog show #f))]))

(define ok-button
  (new button%
       [parent button-hpanel]
       [label "&Ok"]
       [callback
        (λ (button event)
          (unless (string-null? (send search-tfield get-value))
            (ok-callback)))]))
