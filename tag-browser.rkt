#lang racket/base
; tag-browser.rkt
; browse taglist and images, modify tags if necessary
(require racket/class
         racket/gui/base
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

(define browser-menu-bar-file-rename
  (new menu-item%
       [parent browser-menu-bar-file]
       [label "Rename Tag"]
       [help-string "Rename tag and refresh browser."]
       [callback (λ (button evt)
                   (define sel (send tag-lbox get-selection))
                   (cond [(number? sel)
                          (define tag-label (send tag-lbox get-string sel))
                          (send dialog-tfield set-value tag-label)
                          (send rename-dialog show #t)]
                         [else
                          (message-box "Ivy Tag Browser - Error"
                                       "You must first select a tag to rename."
                                       #f
                                       '(ok stop))]))]))

(define rename-dialog
  (new dialog%
       [label "Ivy Tag Browser - Rename"]
       [width 400]
       [height 200]))

(define dialog-tfield
  (new text-field%
       [parent rename-dialog]
       [label ""]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'tfield-enter)
                     (eprintf "dialog-tfield pressed enter!\n")))]))

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
       [alignment '(center center)]))

(define (thumb-callback button event)
  (void))

(define thumb-bmp (make-object bitmap% 100 100))

(define (update-tag-browser)
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
    (send tag-lbox append tag)))

(define (show-tag-browser)
  (update-tag-browser)
  (send browser-frame show #t))
