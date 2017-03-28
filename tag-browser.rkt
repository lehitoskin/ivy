#lang racket/base
; tag-browser.rkt
; browse taglist and images, modify tags if necessary
(require racket/class
         racket/gui/base
         racket/string
         "base.rkt"
         "db.rkt"
         "embed.rkt"
         "files.rkt")
(provide browser-frame show-tag-browser)

(define browser-frame
  (new frame%
       [label "Ivy Tag Browser"]
       [width 800]
       [height 500]))

; set the icon for the frame
(unless (macosx?)
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
               "You must first select a item from the list."
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
         (define imagelist (map path->string (search-db-exact 'or (list old-tag-label))))
         (for ([img (in-list imagelist)])
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
                 (define img-label (send img-lbox get-string sel))
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
  (define img (send lbox get-string sel))
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
                   ; clear old data
                   (send img-lbox clear)
                   (for ([img (in-list (search-db-exact 'or (list tag-label)))])
                     (define img-str (path->string img))
                     (send img-lbox append img-str)))]))

(define img-vpanel
  (new vertical-panel%
       [parent browser-hpanel]))

(define img-lbox
  (new list-box%
       [label "Image List"]
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
                    (pfs (path-files))
                    (send (ivy-tag-tfield) set-field-background color-white)
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

(define (update-tag-browser)
  (send updating-frame center 'both)
  (send updating-frame show #t)
  ; remove the "" we put as a placeholder
  (send tag-lbox clear)
  (send img-lbox clear)
  ; remove old thumb-button
  (remove-children thumb-vpanel (send thumb-vpanel get-children))
  ; get every tag in the database
  (define tag-labels (sort (table-column 'tags 'Tag_Label) string<?))
  ; add them to the list-box
  (for ([tag (in-list tag-labels)])
    (send tag-lbox append tag))
  (send updating-frame show #f))

(define (show-tag-browser)
  (update-tag-browser)
  (unless (send browser-frame is-shown?)
    (send browser-frame center 'both)
    (send browser-frame show #t)))
