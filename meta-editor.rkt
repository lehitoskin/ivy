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

(define attrs '("" "xml:language"))

(define (create-dc-meta type elems [attrs ""])
  (define attr-sel (send attr-choice get-string-selection))
  (define attr-lst
    (for/list ([attr (string-split attrs ",")])
      (list (string->symbol attr-sel) attr)))
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
         (define attr-lst
           (for/list ([attr (string-split attrs ",")])
             (list (string->symbol (send attr-choice get-string-selection)) attr)))
         (if (string-null? attr-sel)
             (append seq `((rdf:li () ,(string-trim v))))
             (append seq `((rdf:li ,attr-lst ,(string-trim v)))))))
     (txexpr (string->symbol type) '() (list lst))]
    ; alt
    [("dc:description" "dc:rights" "dc:title")
     (define lst
       (for/fold ([alt '(rdf:Alt ())])
                 ([v (string-split elems ",")])
         (define attr-lst
           (for/list ([attr (string-split attrs ",")])
             (list (string->symbol (send attr-choice get-string-selection)) attr)))
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
  (define type (send dc-choice get-string-selection))
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
  ; set fields to defaults
  (send dc-tfield set-value "")
  (send attr-tfield set-value "")
  (send dc-choice set-selection 11)
  (send attr-choice set-selection 0))

(define meta-frame
  (new frame%
       [label "Ivy Metadata Editor"]
       [width 600]
       [height 200]))
(void (send meta-frame set-icon (read-bitmap logo)))

(define dc-hpanel
  (new horizontal-panel%
       [parent meta-frame]
       [alignment '(left center)]))

; on callback, update the dc-tfield with the data
; from XMP, if available
(define dc-choice
  (new choice%
       [parent dc-hpanel]
       [label "XMP Tags    "]
       [choices (append dublin-core xmp-base)]
       [selection 11]
       [stretchable-height #f]
       [callback
        (λ (choice evt)
          ; when the choice is selected, fill the tfield with its contents
          (define sel (string->symbol (send choice get-string-selection)))
          (define xexpr (string->xexpr (if (empty? (image-xmp))
                                           ""
                                           (first (image-xmp)))))
          (define found (findf*-txexpr xexpr (is-tag? sel)))
          (cond [found
                 (case sel
                   ; all the Bag's, Seq's, and Alt's
                   [(dc:contributor
                     dc:creator
                     dc:date
                     dc:language
                     dc:publisher
                     dc:relation
                     dc:rights
                     dc:subject
                     dc:title
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
                    (send dc-tfield set-value (string-join lst ", "))])]
                [else (send dc-tfield set-value "")])
          (send dc-tfield refresh))]))

(define dc-tfield
  (new text-field%
       [parent dc-hpanel]
       [label #f]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (ok-callback)))]))

(define attr-hpanel
  (new horizontal-panel%
       [parent meta-frame]
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
       [parent meta-frame]
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
  (send meta-frame show #t))
