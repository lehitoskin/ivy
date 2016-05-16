#lang racket/base
; search-dialog.rkt
(require racket/dict
         racket/gui/base
         racket/class
         racket/string
         racket/list
         "base.rkt"
         "search-results.rkt")
(provide search-tag-dialog
         search-tfield)

(define (ok-callback)
  (send search-tag-dialog show #f)
  (define tags
    (if (string=? (send search-tfield get-value) "")
        #f
        (sort (string-split (send search-tfield get-value) ", ") string<?)))
  (define search-type
    (string->symbol
     (send type-rbox get-item-label
           (send type-rbox get-selection))))
  ; make sure there aren't any nonexistant files in the dictionary
  (clean-dict! master)
  (define imgs
    (if tags
        (search-dict master search-type tags)
        (dict-keys master)))
  (define exclude-tags
    (if (string=? (send exclude-tfield get-value) "")
        #f
        (sort (string-split (send exclude-tfield get-value) ", ") string<?)))
  (cond [(empty? imgs)
         (display-nil-results-alert)
         (send (send search-tfield get-editor) select-all)
         (send search-tag-dialog show #t)]
        [else
         (if exclude-tags
             (display-tags (exclude-search master imgs exclude-tags))
             (display-tags imgs))]))

(define search-tag-dialog
  (new dialog%
       [label "Ivy - Search Tags"]
       [width 400]
       [height 100]
       [style '(close-button)]))

(define search-tfield
  (new text-field%
       [parent search-tag-dialog]
       [label "Search for tags: "]
       [callback
        (λ (tf evt)
          (when (and
                 (eq? (send evt get-event-type) 'text-field-enter)
                 (not (string=? (send tf get-value) "")))
            (ok-callback)))]))

(define type-rbox
  (new radio-box%
       [parent search-tag-dialog]
       [label "Search type"]
       [choices '("or" "and")]))

(define exclude-tfield
  (new text-field%
       [parent search-tag-dialog]
       [label "Exclude tags: "]
       [callback
        (λ (tf evt)
          (when (and
                 (eq? (send evt get-event-type) 'text-field-enter)
                 (not (string=? (send tf get-value) "")))
            (ok-callback)))]))

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
          (unless (string=? (send search-tfield get-value) "")
            (ok-callback)))]))
