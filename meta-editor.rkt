#lang racket/base
; meta-editor.rkt
; edit the XMP metadata of a file in more depth
; than just the dc:subject entries
(require racket/class
         racket/date
         racket/gui/base
         racket/list
         racket/string
         (only-in srfi/13 string-null?)
         txexpr
         "base.rkt"
         "embed.rkt"
         "files.rkt")
(provide show-meta-frame)

(define xmp-base
  '(; unordered list of properties edited outsite this program
    ; rdf:Bag
    "xmp:Advisory"
    ; base URL for relative links contained within the resource
    ; Text
    "xmp:BaseURL"
    ; date and time the resource was originally created
    ; Text
    "xmp:CreateDate"
    ; name of the first known tool used to create the resource
    ; Text
    "xmp:CreatorTool"
    ; unordered list of unique identifiers within a given context
    ; rdf:Bag
    "xmp:Identifier"
    ; short word or phrase that identifies a document as a member
    ; of a user-defined collection
    ; Text
    "xmp:Label"
    ; the date and time that any metadata for this resource was last changed.
    ; should be the same as or more recent than xmp:ModifyDate
    ; Text
    ; no point in editing this part, because it'll just get overwritten anyway
    ;"xmp:MetadataDate"
    ;
    ; the date and time the resource was last modified (i.e. in GIMP)
    ; Text
    ; no point in editing this part either
    ;"xmp:ModifyDate"
    ; a short informal name for the resource
    ; Text
    "xmp:Nickname"
    ; a number that indicates a document's status relative to other documents
    ; Text
    "xmp:Rating"
    ; an alternative array of thumbnail images for a file
    ; Text
    "xmp:Thumbnails"))

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

#|(define (quotify str)
  (define len (string-length str))
  (define char0 (string-ref str 0))
  (define charn (string-ref str (- len 1)))
  (cond [(and (char=? char0 #\") (char=? charn #\")) str]
        [(and (char=? char0 #\") (not (char=? charn #\")))
         (string-append str "\"")]
        [(and (not (char=? char0 #\")) (char=? charn #\"))
         (string-append "\"" str)]
        [else (string-append "\"" str "\"")]))

(define is-dc?
    (for/hash ([dc (in-list dublin-core)])
      (values dc (is-tag? (string->symbol dc)))))|#

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
         (txexpr (string->symbol type) attr-lst (list elems)))]))

(define (ok-callback)
  (define type (send dc-choice get-string-selection))
  (define elems (send dc-tfield get-value))
  (define attrs (send attr-tfield get-value))
  (cond
    ; if elem-tfield is empty, remove the values for that tag
    [(string-null? elems)]
    [else
     (println (create-dc-meta type elems attrs))])
  ; set fields to defaults
  (send dc-tfield set-value "")
  (send attr-tfield set-value "")
  (send dc-choice set-selection 0)
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
       [label "XMP Tags "]
       [choices (append xmp-base dublin-core)]
       [stretchable-height #f]))

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
       [label "Attribute val   "]
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
                   ; set fields to defaults
                   (send dc-tfield set-value "")
                   (send attr-tfield set-value "")
                   (send dc-choice set-selection 0)
                   (send attr-choice set-selection 0)
                   (send meta-frame show #f))]))

(define ok-button
  (new button%
       [parent button-hpanel]
       [label "&Ok"]
       [callback (λ (button event) (ok-callback))]))

(define (show-meta-frame)
  ; send the xmp tags to dc-tfield
  ; send the xmp attrs to attr-tfield
  (send meta-frame show #t))
