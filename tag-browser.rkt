#lang racket/base
; tag-browser.rkt
; browse taglist and images, modify tags if necessary
(require racket/class
         racket/gui/base
         racket/string
         (only-in srfi/13 string-contains-ci)
         "base.rkt"
         "db.rkt"
         "embed.rkt"
         "files.rkt"
         "thumbnails.rkt")
(provide browser-frame show-tag-browser)

(define browser-frame
  (new frame%
       [label "Ivy Tag Browser"]
       [width 800]
       [height 500]))

; set the icon for the frame
(unless macosx?
  (void (send browser-frame set-icon logo-bmp)))

; begin menu bar definitions

(define browser-menu-bar
  (new menu-bar%
       [parent browser-frame]))

(define browser-menu-bar-file
  (new menu%
       [parent browser-menu-bar]
       [label "&File"]))

(define browser-menu-bar-edit
  (new menu%
       [parent browser-menu-bar]
       [label "&Edit"]))

(define browser-menu-bar-file-close
  (new menu-item%
       [parent browser-menu-bar-file]
       [label "Close"]
       [shortcut #\W]
       [callback (λ (button evt)
                   (send browser-frame show #f))]))

(define (err-mbox)
  (message-box "Ivy Tag Browser - Error"
               "You must first select an item from the list."
               #f
               '(ok stop)))

(define browser-menu-bar-edit-rename
  (new menu-item%
       [parent browser-menu-bar-edit]
       [label "Rename Tag"]
       [help-string "Rename tag and refresh browser."]
       [callback (λ (button evt)
                   (define sel (send tag-lbox get-selection))
                   (cond [(number? sel)
                          (define tag-label (send tag-lbox get-string sel))
                          (send dialog-tfield set-value tag-label)
                          (send dialog-tfield refresh)
                          (send dialog-tfield focus)
                          (send rename-dialog center 'both)
                          (send rename-dialog show #t)]
                         [else (err-mbox)]))]))

(define rename-dialog
  (new dialog%
       [label "Ivy Tag Browser - Rename"]
       [width 400]
       [height 100]))

(define dialog-hpanel
  (new horizontal-panel% [parent rename-dialog]))

(define (rename-ok-callback tfield)
  (define sel (send tag-lbox get-selection))
  (define old-tag-label (send tag-lbox get-string sel))
  ; scrub the new tag label of any commas
  (define new-tag-label (string-replace (send tfield get-value) "," ""))
  (cond [(string-null? new-tag-label)
         (message-box "Ivy Tag Browser - Error"
                      "The new tag must not be empty!"
                      #f
                      '(ok stop))]
        [else
         (printf "Changing tag label from ~v to ~v\n" old-tag-label new-tag-label)
         ; get the image list from the old tag
         (define img-lst (map path->string (search-db-exact 'or (list old-tag-label))))
         (for ([img (in-list img-lst)])
           (add-tags! img (list new-tag-label))
           (del-tags! img (list old-tag-label))
           (when (embed-support? img)
             (add-embed-tags! img (list new-tag-label))
             (del-embed-tags! img (list old-tag-label))))
         (send rename-dialog show #f)
         (update-tag-browser)]))

(define dialog-tfield
  (new text-field%
       [parent dialog-hpanel]
       [label ""]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (rename-ok-callback tfield)))]))

(define dialog-button
  (new button%
       [parent dialog-hpanel]
       [label "Ok"]
       [callback (λ (button evt)
                   (rename-ok-callback dialog-tfield))]))

(define browser-menu-bar-edit-delete
  (new menu-item%
       [parent browser-menu-bar-edit]
       [label "Delete Tag"]
       [help-string "Delete tag and refresh browser."]
       [callback
        (λ (button evt)
          (define sel (send tag-lbox get-selection))
          (cond [(number? sel)
                 (define tag-label (send tag-lbox get-string sel))
                 ; make certain we want to delete this tag
                 (define ok-cancel
                   (message-box "Ivy Tag Browser - Delete Tag"
                                "Are you sure you want to delete this tag?"
                                #f
                                '(ok-cancel caution)))
                 (when (eq? ok-cancel 'ok)
                   (define imagelist
                     (map path->string (search-db-exact 'or (list tag-label))))
                   (for ([img (in-list imagelist)])
                     (del-tags! img (list tag-label))
                     (when (embed-support? img)
                       (del-embed-tags! img (list tag-label))))
                   (update-tag-browser))]
                [else (err-mbox)]))]))

(define browser-menu-bar-separator
  (new separator-menu-item%
       [parent browser-menu-bar-edit]))

(define browser-menu-bar-edit-edit
  (new menu-item%
       [parent browser-menu-bar-edit]
       [label "Edit Image Tags"]
       [help-string "Edit the taglist of the selected image."]
       [callback
        (λ (button evt)
          (define sel (send img-lbox get-selection))
          (cond [(number? sel)
                 (define img-label (send img-lbox get-data sel))
                 ; 15 the tallest any column can be
                 (define tag-grid (grid-list (image-taglist img-label) 15))
                 ; remove any children vpanel might have
                 (remove-children edit-tags-check-hpanel (send edit-tags-check-hpanel get-children))
                 ; loop over the tag sections
                 (for ([tag-section (in-list tag-grid)])
                   (define vpanel-section
                     (new vertical-panel%
                          [parent edit-tags-check-hpanel]
                          [alignment '(left top)]))
                   ; add check boxes to the vpanel
                   (for ([tag (in-list tag-section)])
                     (new check-box%
                          [label tag]
                          [parent vpanel-section]
                          [value #t]
                          [callback
                           (λ (button evt)
                             (cond [(send button get-value)
                                    (add-tags! img-label tag)
                                    (when (embed-support? img-label)
                                      (add-embed-tags! img-label tag))]
                                   [else
                                    (del-tags! img-label (list tag))
                                    (when (embed-support? img-label)
                                      (del-embed-tags! img-label tag))]))])))
                 (send edit-tags-dialog center 'both)
                 (send edit-tags-dialog show #t)]
                [else (err-mbox)]))]))

(define edit-tags-dialog
  (new dialog%
       [label "Ivy Tag Browser - Edit Tags"]
       [width 200]
       [height 400]))

(define edit-tags-check-hpanel
  (new horizontal-panel%
       [parent edit-tags-dialog]))

(define edit-tags-new-hpanel
  (new horizontal-panel%
       [parent edit-tags-dialog]))

(define (edit-tags-callback lbox tfield)
  (define sel (send lbox get-selection))
  (define img (send lbox get-data sel))
  (define tags (send tfield get-value))
  ; empty tag string means add no new tags
  (unless (string-null? tags)
    ; turn the string of tag(s) into a list then sort it
    (define tag-lst (sort (tfield->list tfield) string<?))
    (add-tags! img tag-lst)
    (when (embed-support? img)
      (add-embed-tags! img tag-lst))
    (send tfield set-value ""))
  (send edit-tags-dialog show #f)
  (update-tag-browser))

(define edit-tags-tfield
  (new text-field%
       [parent edit-tags-new-hpanel]
       [label "New tag(s): "]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (edit-tags-callback img-lbox tfield)))]))

(define edit-tags-button
  (new button%
       [parent edit-tags-new-hpanel]
       [label "Ok"]
       [callback (λ (button evt)
                   (edit-tags-callback img-lbox edit-tags-tfield))]))

; end menu bar definitions

; begin tag filtering/search definitions

(define use-regex? (make-parameter #f))

(define (filter-query tfield)
  (or (send (send tfield get-editor) get-text)
      (if (use-regex?)
          ".*"
          "")))

(define tag-filter-layout
  (new horizontal-panel%
       [parent browser-frame]
       [stretchable-height #f]))

(define (filter-tags filter-str regex)
  (λ (tag)
    (if (use-regex?)
        (regexp-match filter-str tag)
        (string-contains-ci tag filter-str))))

(define tag-filter-tfield
  (new text-field%
       [parent tag-filter-layout]
       [label "Filter Tags"]
       [callback (λ (tfield evt)
                   (update-tag-browser (filter-query tfield)))]))

(define tag-filter-regex-checkbox
  (new check-box%
       [parent tag-filter-layout]
       [label "Regex"]
       [value #f]
       [callback (λ (chk evt)
                   (use-regex? (not (use-regex?)))
                   (update-tag-browser))]))

; end tag filtering/search definitions

(define browser-hpanel
  (new horizontal-panel%
       [parent browser-frame]))

(define tag-vpanel
  (new vertical-panel%
       [parent browser-hpanel]))

(define tag-lbox
  (new list-box%
       [label "Tag List"]
       [parent tag-vpanel]
       [style '(single vertical-label)]
       [choices (list "")]
       [callback (λ (lbox evt)
                   (define sel (send lbox get-selection))
                   (define tag-label (if sel (send lbox get-string sel) ""))
                   (define img-lst (map path->string (search-db-exact 'or (list tag-label))))
                   (send img-lbox set-label (format "Image List (~a)" (length img-lst)))
                   (send img-lbox clear)
                   (remove-children thumb-vpanel (send thumb-vpanel get-children))
                   ; add paths to the image lbox, truncating if necessary
                   (send img-lbox set
                         (for/list ([img (in-list img-lst)])
                           (string-truncate img +label-max+)))
                   ; add full path string data to the entry
                   (for ([img (in-list img-lst)]
                         [n (in-naturals)])
                     (send img-lbox set-data n img))
                   ; double click to load the tag category
                   (when (eq? (send evt get-event-type) 'list-box-dclick)
                     (define img-path (string->path (send img-lbox get-data 0)))
                     (define-values (base name dir?) (split-path img-path))
                     (image-dir base)
                     ; populate pfs with the images in the tag category
                     (define lst
                       (for/list ([n (in-range (send img-lbox get-number))])
                         (string->path (send img-lbox get-data n))))
                     (pfs lst)
                     (send (ivy-tag-tfield) set-default-background)
                     (image-path img-path)
                     (load-image img-path)))]))

(define img-vpanel
  (new vertical-panel%
       [parent browser-hpanel]))

(define img-lbox
  (new list-box%
       [label "Image List        "]
       [parent img-vpanel]
       [style '(single vertical-label)]
       [choices (list "")]
       [callback
        (λ (lbox evt)
          (define sel (send lbox get-selection))
          ; if sel is #f, make img-str ""
          (define img-str (if sel (send lbox get-string sel) ""))
          ; do nothing if the user clicks on the placeholder string
          (unless (string-null? img-str)
            (define img-path (string->path img-str))
            ; set new thumbnail data
            ; get the thumbnail path
            (define thumb-path (path->md5 img-str))
            ; make certain the thumbnail exists
            (unless (file-exists? thumb-path)
              (generate-thumbnails (list img-str)))
            (send thumb-bmp load-file thumb-path)
            ; remove old thumb-button
            (remove-children thumb-vpanel (send thumb-vpanel get-children))
            ; generate new thumb-button
            (new button%
                 [parent thumb-vpanel]
                 [label thumb-bmp]
                 [callback
                  (λ (button evt)
                    (define-values (base name dir?) (split-path img-path))
                    (image-dir base)
                    ; populate pfs with the images in the tag category
                    (define lst
                      (for/list ([img (in-range (send lbox get-number))])
                        (string->path (send lbox get-data img))))
                    (pfs lst)
                    (send (ivy-tag-tfield) set-default-background)
                    (image-path img-path)
                    (load-image img-path))])))]))

(define thumb-vpanel
  (new vertical-panel%
       [parent browser-hpanel]
       [alignment '(center center)]
       [stretchable-width #f]))

(define thumb-bmp (make-object bitmap% 128 128))

(define updating-frame
  (new frame%
       [label "Ivy Tag Browser"]
       [width 200]
       [height 40]
       [style '(float)]))

(define updating-message
  (new message%
       [parent updating-frame]
       [label "Updating Tag Browser..."]))

(define (update-tag-browser [filter-str (filter-query tag-filter-tfield)])
  (send updating-frame center 'both)
  (send updating-frame show #t)
  ; remove the "" we put as a placeholder
  (send tag-lbox clear)
  (send img-lbox clear)
  ; remove old thumb-button
  (remove-children thumb-vpanel (send thumb-vpanel get-children))
  ; get every tag in the database
  (define tag-labels (sort
                      (filter (filter-tags filter-str use-regex?)
                              (table-column 'tags 'Tag_Label))
                      string<?))
  ; add them to the list-box, truncating if necessary
  (send tag-lbox set
        (for/list ([img (in-list tag-labels)])
          (string-truncate img +label-max+)))
  ; set data for the unmodified label string
  (for ([img (in-list tag-labels)]
        [n (in-naturals)])
    (send tag-lbox set-data n img))
  (send updating-frame show #f))

(define (show-tag-browser)
  (update-tag-browser)
  (unless (send browser-frame is-shown?)
    (send browser-frame center 'both)
    (send tag-filter-tfield focus)
    ; select filter contents on-show
    (let ([txt (send tag-filter-tfield get-editor)])
      (send txt move-position 'home)
      (send txt move-position 'end #t))
    (send browser-frame show #t)))
