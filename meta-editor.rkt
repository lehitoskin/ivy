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
         "embed.rkt"
         "files.rkt")
(provide show-meta-frame)

; these are actually located as attributes for rdf:Description
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

(define (create-dc-meta type elems [attrs ""])
  (define attr-sel (send attr-choice get-string-selection))
  (define attr-lst `(,(list (string->symbol attr-sel) attrs)))
  (case type
    ; bag - unsorted number of entries
    [("dc:contributor" "dc:language" "dc:publisher"
                       "dc:relation" "dc:subject" "dc:type")
     (define lst
       (for/fold ([bag '(rdf:Bag ())])
                 ([v (string-split elems ",")])
         (if (string-null? attr-sel)
             (append bag `((rdf:li () ,(string-trim v))))
             (append bag `((rdf:li ,attr-lst ,(string-trim v)))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; seq - sorted number of entries
    [("dc:creator" "dc:date")
     (define lst
       (for/fold ([seq '(rdf:Seq ())])
                 ([v (string-split elems ",")])
         (if (string-null? attr-sel)
             (append seq `((rdf:li () ,(string-trim v))))
             (append seq `((rdf:li ,attr-lst ,(string-trim v)))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; alt
    [("dc:description" "dc:rights" "dc:title")
     (define lst
       (for/fold ([alt '(rdf:Alt ())])
                 ([v (string-split elems ",")])
         (if (string-null? attr-sel)
             (append alt `((rdf:li () ,(string-trim v))))
             (append alt `((rdf:li ,attr-lst ,(string-trim v)))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; text - single string entry
    [("dc:coverage" "dc:identifier" "dc:source")
     (if (string-null? attr-sel)
         (txexpr (string->symbol type) '() (list elems))
         (txexpr (string->symbol type) attr-lst (list elems)))]
    ; attr stuff
    [("xmp:BaseURL" "xmp:Label" "xmp:Rating")
     (define xexpr (string->xexpr (if (empty? (image-xmp))
                                      ""
                                      (first (image-xmp)))))
     ; these go inside rdf:Description as attrs
     (define rdf:desc (findf-txexpr xexpr is-rdf:Description?))
     (attr-set rdf:desc (string->symbol type) elems)]
    [else empty]))

(define (ok-callback)
  (define type (send xmp-lbox get-string-selection))
  (define elems (send dc-tfield get-value))
  (define attrs (send attr-tfield get-value))
  ; set the new information
  (define setted
    (case type
      [("xmp:BaseURL" "xmp:Label" "xmp:Rating")
       ((set-xmp-tag 'rdf:Description)
        (string->xexpr (if (empty? (image-xmp))
                           ""
                           (first (image-xmp))))
        (create-dc-meta type elems attrs))]
      [else
       ((set-xmp-tag (string->symbol type))
        (string->xexpr (if (empty? (image-xmp))
                           ""
                           (first (image-xmp))))
        (create-dc-meta type elems attrs))]))
  (image-xmp (list (xexpr->string setted)))
  (set-embed-xmp! (image-path) (first (image-xmp)))
  (send meta-frame show #f))

(define (fields-defaults)
  ; set tabs to default
  (let ([num (send tab-panel get-number)])
    (cond [(= num 1)
           (send tab-panel set-item-label 0 "default")]
          [else
           ; delete all but one tab
           (for ([tab (in-range (- num 1))])
             (send tab-panel delete 1))
           (send tab-panel set-item-label 0 "default")]))
  ; set fields to defaults
  (send dc-tfield set-value "")
  (send attr-tfield set-value "")
  (send xmp-lbox set-string-selection "dc:subject")
  (send attr-choice set-string-selection ""))

(define (langs-hash found)
  (define elem+attrs (map (λ (tx) (findf-txexpr tx is-rdf:li?)) found))
  (for/hash ([tx (in-list elem+attrs)])
    (define elem (first (get-elements tx)))
    (define lang
      (first
       (filter
        (λ (pair)
          (equal? (first pair) 'xml:lang))
        (get-attrs tx))))
    (values (second lang) elem)))

(define meta-frame
  (new frame%
       [label "Ivy Metadata Editor"]
       [width 600]
       [height 400]))
(void (send meta-frame set-icon (read-bitmap logo)))

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
                      (define xexpr (string->xexpr (if (empty? (image-xmp))
                                                       ""
                                                       (first (image-xmp)))))
                      (define found (findf*-txexpr xexpr (is-tag? sel)))
                      (when found
                        (define elem+attrs (map (λ (tx) (findf-txexpr tx is-rdf:li?)) found))
                        (define attrs (map (λ (rdf:li) (get-attrs rdf:li)) elem+attrs))
                        (define elems (map (λ (rdf:li) (get-elements rdf:li)) elem+attrs))
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
                   (define str (send lbox get-string-selection))
                   (define sel (string->symbol str))
                   (define xexpr (string->xexpr (if (empty? (image-xmp))
                                                    ""
                                                    (first (image-xmp)))))
                   (define found (findf*-txexpr xexpr (is-tag? sel)))
                   (unless found
                     (send dc-tfield set-value ""))
                   ; set tabs to default
                   (let ([num (send tab-panel get-number)])
                     (cond [(= num 1)
                            (send tab-panel set-item-label 0 "default")]
                           [else
                            ; delete all but one tab
                            (for ([tab (in-range (- num 1))])
                              (send tab-panel delete 1))
                            (send tab-panel set-item-label 0 "default")]))
                   (when found
                     (case sel
                       ; all the Alt's
                       [(dc:description dc:rights dc:title)
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
                        (send dc-tfield set-value (hash-ref langs tab-label))]
                       ; all the Bag's and Seq's
                       [(dc:contributor
                         dc:creator
                         dc:date
                         dc:language
                         dc:publisher
                         dc:relation
                         dc:subject
                         dc:type)
                        (define rdf:li (findf*-txexpr (first found) is-rdf:li?))
                        (define lst (flatten (map (λ (item) (get-elements item)) rdf:li)))
                        (send dc-tfield set-value (string-join lst ", "))]
                       [(xmp:BaseURL xmp:Label xmp:Rating)
                        (define rdf-desc (findf-txexpr xexpr (is-tag? 'rdf:Description)))
                        (when rdf-desc
                          ; attr may be a number via xmp:Rating
                          (define attr (attr-ref rdf-desc sel (λ _ "")))
                          (define attr-str
                            (if (number? attr)
                                (number->string attr)
                                attr))
                          (send dc-tfield set-value attr-str))]
                       ; everything else is just a single value
                       [else
                        (define lst (flatten (map (λ (item) (get-elements item)) found)))
                        (send dc-tfield set-value (string-join lst ", "))])))]))
                          

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

; on callback, update the attr-tfield with the data
; from XMP, if available
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

(define cancel-button
  (new button%
       [parent button-hpanel]
       [label "&Cancel"]
       [callback (λ (button event)
                   (fields-defaults)
                   (send meta-frame show #f))]))

(define ok-button
  (new button%
       [parent button-hpanel]
       [label "&Set"]
       [callback (λ (button event) (ok-callback))]))

(define (show-meta-frame)
  (fields-defaults)
  (when (and (not (equal? (image-path) root-path))
             (embed-support? (image-path)))
    (define tags (get-embed-tags (image-path)))
    (send dc-tfield set-value (string-join tags ", ")))
  (send dc-tfield refresh)
  (send meta-frame show #t)
  ; eliminate Gtk-WARNING messages
  (when (empty? (send meta-frame get-children))
    (send meta-frame add-child tab-panel)))
