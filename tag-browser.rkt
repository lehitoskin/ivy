#lang racket/base
; tag-browser.rkt
; browse taglist and images, modify tags if necessary
(require racket/class
         racket/gui/base
         racket/string
         (only-in srfi/13 string-null?)
         "base.rkt"
         "db.rkt")
(provide show-tag-browser)

(define browser-frame
  (new frame%
       [label "Ivy Tag Browser"]
       [width 800]
       [height 500]))

(define browser-menu-bar
  (new menu-bar% [parent browser-frame]))

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
               "You must first select a tag from the list."
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
           (remove-img/tags! img (list old-tag-label)))
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
                     (remove-img/tags! img (list tag-label)))
                   (update-tag-browser))]
                [else (err-mbox)]))]))

(define hpanel
  (new horizontal-panel% [parent browser-frame]))

(define tag-vpanel
  (new vertical-panel% [parent hpanel]))

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
  (new vertical-panel% [parent hpanel]))

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
            (define thumb-path (path->thumb-path img-str))
            ; make certain the thumbnail exists
            (unless (file-exists? thumb-path)
              (generate-thumbnails (list img-str)))
            (send thumb-bmp load-file thumb-path)
            ; remove old thumb-button
            (define vpanel-children (send thumb-vpanel get-children))
            (unless (null? vpanel-children)
              (send thumb-vpanel delete-child (car (send thumb-vpanel get-children))))
            ; generate new thumb-button
            (new button%
                 [parent thumb-vpanel]
                 [label thumb-bmp]
                 [callback
                  (λ (button evt)
                    (define-values (base name dir?) (split-path img-path))
                    (image-dir base)
                    (pfs (path-files))
                    (send (ivy-tag-tfield) set-field-background (make-object color% "white"))
                    (image-path img-path)
                    (load-image img-path))])))]))

(define thumb-vpanel
  (new vertical-panel%
       [parent hpanel]
       [alignment '(center center)]
       [stretchable-width #f]))

(define (thumb-callback button event)
  (void))

(define thumb-bmp (make-object bitmap% 100 100))

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
  (send updating-frame show #t)
  ; remove the "" we put as a placeholder
  (send tag-lbox clear)
  (send img-lbox clear)
  ; remove old thumb-button
  (define vpanel-children (send thumb-vpanel get-children))
  (unless (null? vpanel-children)
    (send thumb-vpanel delete-child (car (send thumb-vpanel get-children))))
  ; get every tag in the database
  (define tag-labels (sort (table-column 'tags 'Tag_Label) string<?))
  ; add them to the list-box
  (for ([tag (in-list tag-labels)])
    (send tag-lbox append tag))
  (send updating-frame show #f))

(define (show-tag-browser)
  (update-tag-browser)
  (send browser-frame show #t))
