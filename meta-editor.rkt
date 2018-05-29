#lang racket/base
; meta-editor.rkt
; edit the XMP metadata of a file in more depth
; than just the dc:subject entries
(require racket/class
         racket/gui/base
         racket/list
         racket/string
         txexpr
         xml
         "base.rkt"
         (only-in "db.rkt" reconcile-tags! set-image-rating!)
         "embed.rkt"
         "files.rkt")
(provide create-dc-meta show-meta-frame)

; these are actually located as attributes for rdf:Description
; but may also be regular tags
(define xmp-base
  '(; base URL for relative links contained within the resource
    ; Text
    "xmp:BaseURL"
    ; short word or phrase that identifies a document as a member
    ; of a user-defined collection
    ; Text
    "xmp:Label"
    ; a number that indicates a document's status relative to other documents
    ; Text
    "xmp:Rating"))

(define dublin-core
  '(; contributors to the resource (other than the author)
    ; rdf:Bag
    "dc:contributor"
    ; the spatial applicability of the resource
    ; Text
    "dc:coverage"
    ; the author of the resource
    ; (listed in order of precedence, if significant)
    ; rdf:Seq
    "dc:creator"
    ; Date(s) that something interesting happened to the resource
    ; rdf:Seq
    "dc:date"
    ; A textual description of the resource
    ; rdf:Alt
    "dc:description"
    ; Unique identifier of the resource
    ; Text
    "dc:identifier"
    ; Unordered list of languages used in the resource
    ; rdf:Bag
    "dc:language"
    ; An entity responsible for making the resource available
    ; rdf:Bag
    "dc:publisher"
    ; Relationships to other documents
    ; rdf:Bag
    "dc:relation"
    ; Informal rights statement, selected by language.
    ; rdf:Alt
    "dc:rights"
    ; Unique identifier of the work
    ; Text
    "dc:source"
    ; Unordered list of keywords associated with the resource
    ; rdf:Bag
    "dc:subject"
    ; Title of the work, multiple titles for multiple languages
    ; rdf:Alt
    "dc:title"
    ; A document type (novel, poem, etc)
    ; rdf:Bag
    "dc:type"))

(define attrs '("" "xml:lang"))

(define (create-dc-meta type elems [attrs ""] [img-xmp image-xmp])
  (define attr-sel (send attr-choice get-string-selection))
  (define attr-lst `(,(list (string->symbol attr-sel) attrs)))
  (case type
    ; bag - unsorted number of entries
    [("dc:contributor" "dc:language" "dc:publisher"
                       "dc:relation" "dc:subject" "dc:type")
     (define lst
       (for/fold ([bag '(rdf:Bag ())])
                 ([v (string-split elems ",")])
         (append bag `((rdf:li () ,(string-trim v))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; seq - sorted number of entries
    [("dc:creator" "dc:date")
     (define lst
       (for/fold ([seq '(rdf:Seq ())])
                 ([v (string-split elems ",")])
         (append seq `((rdf:li () ,(string-trim v))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; alt - may have xml:lang attr
    [("dc:description" "dc:rights" "dc:title")
     (define lst
       (if (string-null? attr-sel)
           `(rdf:Alt () (rdf:li () ,(string-trim elems)))
           `(rdf:Alt () (rdf:li ,attr-lst ,(string-trim elems)))))
     (txexpr (string->symbol type) '() (list lst))]
    ; text - single string entry
    [("dc:coverage" "dc:identifier" "dc:source")
     (txexpr (string->symbol type) '() (list elems))]
    ; tags as attrs - may also be represented as elements
    [("xmp:BaseURL" "xmp:Label" "xmp:Rating")
     (define xexpr (if (empty? (unbox img-xmp))
                       (make-xmp-xexpr empty)
                       (string->xexpr (first (unbox img-xmp)))))
     (define type-sym (string->symbol type))
     ; if the tag exists as an element, replace it
     (define xmp (findf-txexpr xexpr (is-tag? type-sym)))
     (cond [xmp (txexpr type-sym '() elems)]
           [else
            ; these go inside rdf:Description as attrs
            (define rdf:desc (findf-txexpr xexpr is-rdf:Description?))
            (attr-set rdf:desc (string->symbol type) elems)])]
    [else empty]))

(define (ok-callback)
  (unless (equal? (image-path) +root-path+)
    (define type (send xmp-lbox get-string-selection))
    (when type
      (define elems (send dc-tfield get-value))
      (define attrs (send attr-tfield get-value))
      ; set the new information
      (define setted
        (case type
          [("xmp:BaseURL" "xmp:Label" "xmp:Rating")
           (define type-sym (string->symbol type))
           (define xexpr (if (empty? (unbox image-xmp))
                             (make-xmp-xexpr empty)
                             (string->xexpr (first (unbox image-xmp)))))
           (define xmp (findf-txexpr xexpr (is-tag? type-sym)))
           ; if the tag exists as an element, replace it
           (cond
             [xmp
              ((set-xmp-tag type-sym)
               xexpr
               (create-dc-meta type elems attrs))]
             ; otherwise set it as an attr
             [else
              ((set-xmp-tag 'rdf:Description)
               xexpr
               (create-dc-meta type elems attrs))])]
          [else
           ((set-xmp-tag (string->symbol type))
            (if (empty? (unbox image-xmp))
                (make-xmp-xexpr empty)
                (string->xexpr (first (unbox image-xmp))))
            (create-dc-meta type elems attrs))]))
      (cond
        ; set the rating in the database
        [(eq? (string->symbol type) 'xmp:Rating)
         (set-image-rating! (path->string (image-path)) (string->number elems))]
        ; set the tags in the database
        [(eq? (string->symbol type) 'dc:subject)
         (reconcile-tags! (path->string (image-path)) (string->taglist elems))])
      (set-box! image-xmp (list (xexpr->xmp setted)))
      (set-embed-xmp! (image-path) (first (unbox image-xmp)))
      (send meta-frame show #f))))

(define (fields-defaults)
  ; set fields to defaults
  (send dc-tfield set-value "")
  (send attr-tfield set-value "")
  (send attr-choice set-string-selection ""))

(define (tab-panel-defaults)
  (define num (send tab-panel get-number))
  (when (> num 1)
    (for ([tab (in-range (- num 1))])
      (send tab-panel delete 1)))
  (send tab-panel set-item-label 0 "default"))

(define (meta-editor-defaults)
  ; set tabs to default
  (tab-panel-defaults)
  (fields-defaults)
  (send xmp-lbox set-string-selection "dc:subject"))

(define (langs-hash found)
  (define elem+attrs (map (λ (tx) (findf*-txexpr tx is-rdf:li?)) found))
  (for/hash ([tx (in-list (first elem+attrs))])
    (define elem (first (rdf:li-fixer (list tx))))
    (define lang
      (first
       (filter
        (λ (pair)
          (eq? (first pair) 'xml:lang))
        (get-attrs tx))))
    (values (second lang) elem)))

(define meta-frame
  (new frame%
       [label "Ivy Metadata Editor"]
       [width 600]
       [height 400]))

(unless macosx?
  (void (send meta-frame set-icon logo-bmp)))

(define tab-panel
  (new tab-panel%
       [parent meta-frame]
       [choices '("default")]
       [style '(no-border deleted)]
       [callback (λ (tpanel evt)
                   (define sel (string->symbol (send xmp-lbox get-string-selection)))
                   (case sel
                     ; all the Alt's
                     [(dc:description dc:rights dc:title)
                      (define xexpr (if (empty? (unbox image-xmp))
                                        (make-xmp-xexpr empty)
                                        (string->xexpr (first (unbox image-xmp)))))
                      (define found (findf*-txexpr xexpr (is-tag? sel)))
                      (when found
                        (define elem+attrs (map (λ (tx) (findf-txexpr tx is-rdf:li?)) found))
                        (define attrs (map get-attrs elem+attrs))
                        (define elems (map get-elements elem+attrs))
                        (define langs (langs-hash found))
                        (define tab-sel (send tpanel get-selection))
                        (define tab-label (send tpanel get-item-label tab-sel))
                        (send attr-tfield set-value tab-label)
                        (send dc-tfield set-value (hash-ref langs tab-label)))]))]))

(define xmp-lbox
  (new list-box%
       [label "XMP Tags"]
       [parent tab-panel]
       [style '(single vertical-label)]
       [choices (append dublin-core xmp-base)]
       [selection 11]
       [callback (λ (lbox evt)
                   (unless (or (equal? (image-path) +root-path+)
                               (not (embed-support? (image-path))))
                     (define str (send lbox get-string-selection))
                     ; just in case get-string-selection returns #f
                     (define sel (string->symbol (if str str "")))
                     (define xexpr (if (empty? (unbox image-xmp))
                                       (make-xmp-xexpr empty)
                                       (string->xexpr (first (unbox image-xmp)))))
                     (define found (findf*-txexpr xexpr (is-tag? sel)))
                     ; set some fields to defaults
                     (fields-defaults)
                     ; set tabs to default
                     (tab-panel-defaults)
                     (case sel
                       ; just in case get-string-selection returns #f
                       [(||) (void)]
                       ; all the Alt's
                       [(dc:description dc:rights dc:title)
                        (when found
                          ; set the tabs in the tab-panel
                          (define langs (langs-hash found))
                          (define langs-lst
                            (sort (hash->list langs) (λ (a b) (string<? (car a) (car b)))))
                          (for ([lang (in-list langs-lst)]
                                [i (in-naturals)])
                            (if (= i 0)
                                (send tab-panel set-item-label i (car lang))
                                (send tab-panel append (car lang))))
                          ; set the attrs choice with our tab selection
                          (send tab-panel set-selection 0)
                          (define tab-label (send tab-panel get-item-label 0))
                          (send attr-choice set-selection 1)
                          (send attr-tfield set-value tab-label)
                          (send dc-tfield set-value (hash-ref langs tab-label)))]
                       ; all the Bag's and Seq's
                       [(dc:contributor
                         dc:creator
                         dc:date
                         dc:language
                         dc:publisher
                         dc:relation
                         dc:subject
                         dc:type)
                        (when found
                          (define rdf:li (findf*-txexpr (first found) is-rdf:li?))
                          (define lst (rdf:li-fixer rdf:li))
                          (send dc-tfield set-value (string-join lst ", ")))]
                       ; grab the attrs from rdf:Description
                       [(xmp:BaseURL xmp:Label xmp:Rating)
                        ; check if it's an attr first
                        (define rdf-desc (findf-txexpr xexpr (is-tag? 'rdf:Description)))
                        (when rdf-desc
                          ; attr may be a number via xmp:Rating
                          (define attr (attr-ref rdf-desc sel (λ _ "")))
                          (define attr-str
                            (if (number? attr)
                                (number->string attr)
                                attr))
                          ; if it doesn't exist as an attr, check if it's an element
                          (if (and found (string-null? attr-str))
                              (send dc-tfield set-value (first (get-elements (first found))))
                              (send dc-tfield set-value attr-str)))]
                       ; everything else is just a single value
                       [else
                        (when found
                          (define lst (rdf:li-fixer found))
                          (send dc-tfield set-value (string-join lst ", ")))])))]))

(define dc-vpanel
  (new vertical-panel%
       [parent tab-panel]
       [stretchable-height #f]))

(define dc-hpanel
  (new horizontal-panel%
       [parent dc-vpanel]
       [alignment '(left center)]))

(define dc-tfield
  (new text-field%
       [parent dc-hpanel]
       [label #f]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (ok-callback)))]))

(define attr-hpanel
  (new horizontal-panel%
       [parent dc-vpanel]
       [alignment '(left center)]))

; on callback, update the attr-tfield with the data from XMP, if available
(define attr-choice
  (new choice%
       [parent attr-hpanel]
       [label "Attribute val "]
       [choices attrs]
       [selection 0]
       [stretchable-height #f]))

(define attr-tfield
  (new text-field%
       [parent attr-hpanel]
       [label #f]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (ok-callback)))]))

(define button-hpanel
  (new horizontal-panel%
       [parent dc-vpanel]
       [alignment '(right center)]))

; make the button position consistent with the host system
; e.g. A Windows host will have the Ok button before Cancel
; while on *NIX it will be the other way around
(void
 (cond [(system-position-ok-before-cancel?)
        (new button%
             [parent button-hpanel]
             [label "&Set"]
             [callback (λ (button event) (ok-callback))])
        (new button%
             [parent button-hpanel]
             [label "&Cancel"]
             [callback (λ (button event)
                         (meta-editor-defaults)
                         (send meta-frame show #f))])]
       [else
        (new button%
             [parent button-hpanel]
             [label "&Cancel"]
             [callback (λ (button event)
                         (meta-editor-defaults)
                         (send meta-frame show #f))])
        (new button%
             [parent button-hpanel]
             [label "&Set"]
             [callback (λ (button event) (ok-callback))])]))

(define (show-meta-frame)
  (meta-editor-defaults)
  (define img (image-path))
  (when (and (not (equal? img +root-path+))
             (embed-support? img))
    ; wait for any previous xmp-threads to complete
    (let loop ()
      (unless (or (not (hash-has-key? xmp-threads img))
                  (thread-dead? (hash-ref xmp-threads img)))
        (printf "Waiting for thread ~a to finish...\n" img)
        (sleep 1/4)
        (loop)))
    (define tags (get-embed-tags img))
    (send dc-tfield set-value (string-join tags ", ")))
  (send dc-tfield refresh)
  (send meta-frame center 'both)
  (send meta-frame show #t)
  ; eliminate Gtk-WARNING messages
  (when (empty? (send meta-frame get-children))
    (send meta-frame add-child tab-panel)))
