#lang racket/base
; db.rkt
; contains class definitions for interacting with racquel and sqlite
(require db/base
         db/sqlite3
         racket/bool
         racket/class
         racket/contract
         racket/format
         racket/list
         racket/string
         racquel
         (only-in srfi/13
                  string-contains-ci)
         "files.rkt")
(provide (all-defined-out)
         disconnect
         make-data-object
         get-column
         keep-duplicates)

(define sqlc
  (sqlite3-connect
   #:database master-file
   #:mode 'create))

; make sure the table and columns exist
(query-exec sqlc
            "create table if not exists tags(Tag_label string not null, Image_List string);")
(query-exec sqlc
            "create table if not exists images(Path string not null, Tag_List string);")

; a single tag class which is associated with a list of images
(define tag%
  (data-class object%
              (table-name "tags")
              (init-column (label "" "Tag_Label"))
              (column (imagelist "" "Image_List")) ; one long list of the images
              (primary-key label)
              (super-new)
              
              (define/public (get-images)
                (define imgs (get-column imagelist this))
                (string-split imgs ","))
              
              ; img is the image's path-string
              ; passing an image% object is a-okay
              (define/public (add-img img)
                (define path
                  (cond [(path? img) (path->string img)]
                        [(string? img) img]
                        [else (get-column path img)]))
                (define il (string-split (get-column imagelist this) ","))
                (unless (member path il)
                  (set! imagelist (string-join (sort (append il (list path)) string<?) ","))))
              
              ; img is the image's path-string
              ; passing an image% object is a-okay
              (define/public (del-img img)
                (define path
                  (cond [(path? img) (path->string img)]
                        [(string? img) img]
                        [else (get-column path img)]))
                (define il (get-images))
                (when (member path il)
                  (set! imagelist (string-join (sort (remove path il) string<?) ","))))))

; a single image class which is associated with a list of tags
; path should be a string
(define image%
  (data-class object%
              (table-name "images")
              (init-column (path "" "Path")) ; string
              (column (taglist "" "Tag_List")) ; one long string of the tags
              (primary-key path)
              (super-new)
              
              (define/public (get-tags)
                (define tags (get-column taglist this))
                (string-split tags ","))
              
              ; tag is the tag's label
              ; passing a tag% object is a-okay
              (define/public (add-tag tag)
                (define label (if (string? tag) tag (get-column label tag)))
                (define tl (string-split (get-column taglist this) ","))
                (unless (member label tl)
                  (set! taglist (string-join (sort (append tl (list label)) string<?) ","))))
              
              ; passing a tag% object is a-okay
              (define/public (del-tag tag)
                (cond
                  ; pass a whole list of tags to remove at once
                  [(list? tag)
                   (define labels
                     (for/list ([t (in-list tag)])
                       (if (string? t)
                           t
                           (get-column label t))))
                   (define old-tags (get-tags))
                   (set! taglist (string-join (sort (remove* labels old-tags) string<?) ","))]
                  ; pass a single tag to remove
                  [else
                   (define label
                     (if (string? tag)
                         tag
                         (get-column label tag)))
                   (define old-tags (get-tags))
                   (when (member label old-tags)
                     (set! taglist (string-join (sort (remove label old-tags) string<?) ",")))]))))


; query:
; -> (rows-result (headers rows))
; headers: (listof any/c)
; rows: (listof vector?)

(define/contract (table-pairs #:db-conn [db-conn sqlc] table)
  (->* ([or/c 'images 'tags])
       (#:db-conn connection?)
       (or/c (listof (list/c path? (listof string?)))
             (listof (list/c string? (listof path?)))
             empty?))
  (case table
    [(images)
     (for/list ([img (select-data-objects db-conn image%)])
       (list (string->path (get-column path img))
             (send img get-tags)))]
    [(tags)
     (for/list ([tag (select-data-objects db-conn tag%)])
       (list (get-column label tag)
             (map string->path (send tag get-images))))]))

; table: (or/c 'images 'tags)
; -> sequence?
(define/contract (in-table-pairs #:db-conn [db-conn sqlc] table)
  (->* ([or/c 'images 'tags]) (#:db-conn connection?) sequence?)
  (in-list (table-pairs #:db-conn db-conn table)))

; table: (or/c 'images 'tags)
(define/contract (table-column #:db-conn [db-conn sqlc] table col)
  (->* ([or/c 'images 'tags]
        [or/c 'Path 'Tag_List 'Tag_Label 'Image_List])
       (#:db-conn connection?)
       (or/c (listof path?) empty?))
  (define result (rows-result-rows (query db-conn (format "select ~a from ~a;" col table))))
  (map string->path (flatten (map vector->list result))))

(define/contract (in-table-column #:db-conn [db-conn sqlc] table col)
  (->* ([or/c 'images 'tags]
        [or/c 'Path 'Tag_List 'Tag_Label 'Image_List])
       (#:db-conn connection?)
       sequence?)
  (in-list (table-column #:db-conn db-conn table col)))

(define/contract (db-has-key? #:db-conn [db-conn sqlc] table key)
  (->* ([or/c 'images 'tags] string?) (#:db-conn connection?) boolean?)
  (define objs
    (case table
      [(images) (select-data-objects db-conn image% (where (= path ?)) key)]
      [(tags) (select-data-objects db-conn tag% (where (= label ?)) key)]))
  (not (empty? objs)))

; add tags to image, add image to tags
; if the image or tags are new, insert them into the database
(define/contract (add-tags! #:db-conn [db-conn sqlc] img tag-lst)
  (->* ([or/c string? data-object?]
        [listof string?])
       (#:db-conn connection?)
       void?)
  ; if the path already exists, grab it
  ; otherwise make a new data-object
  (define img-obj
    (cond [(data-object? img) img]
          [(db-has-key? #:db-conn db-conn 'images img)
           (make-data-object db-conn image% img)]
          [else (new image% [path img])]))
  (for ([tag (in-list tag-lst)])
    ; add each tag to the image% object
    (send img-obj add-tag tag)
    (define tag-obj
      (if (db-has-key? #:db-conn db-conn 'tags tag)
          (make-data-object db-conn tag% tag)
          (new tag% [label tag])))
    ; add the image to each tag% object
    (send tag-obj add-img img)
    (save-data-object db-conn tag-obj))
  (save-data-object db-conn img-obj))

; remove the tags from the img entry
; if img has no tags, remove from db
(define/contract (remove-tags! #:db-conn [db-conn sqlc] img tag-lst)
  (->* ([or/c string? data-object?]
        [listof string?])
       (#:db-conn connection?)
       void?)
  (define img-obj
    (cond [(data-object? img) img]
          [(db-has-key? #:db-conn db-conn 'images img)
           (make-data-object db-conn image% img)]
          [else #f]))
  (when img-obj
    ; remove the tags from the image
    (send img-obj del-tag tag-lst)
    ; if the image has no tags, remove from database
    (if (empty? (send img-obj get-tags))
        (delete-data-object db-conn img-obj)
        ; save the changes made
        (save-data-object db-conn img-obj))))

; tail-recursive remove img from the tag entries
; if the tag has no imgs, remove from db
(define/contract (remove-image! #:db-conn [db-conn sqlc] img tag-lst)
  (->* ([or/c string? data-object?]
        [listof string?])
       (#:db-conn connection?)
       void?)
  (cond [(empty? tag-lst) (void)]
        [(db-has-key? #:db-conn db-conn 'tags (first tag-lst))
         (define tag-obj (make-data-object db-conn tag% (first tag-lst)))
         (send tag-obj del-img img)
         (if (empty? (send tag-obj get-images))
             (delete-data-object db-conn tag-obj)
             (save-data-object db-conn tag-obj))
         (remove-image! #:db-conn db-conn img (rest tag-lst))]))

(define/contract (remove-img/tags! #:db-conn [db-conn sqlc] img tag-lst)
  (->* ([or/c string? data-object?]
        [listof string?])
       (#:db-conn connection?)
       void?)
  (remove-tags! #:db-conn db-conn img tag-lst)
  (remove-image! #:db-conn db-conn img tag-lst))

; remove img from images and all references from tags
(define/contract (db-purge! #:db-conn [db-conn sqlc] img)
  (->* (path-string?)
       (#:db-conn connection?)
       void?)
  (define img-str (if (path? img) (path->string img) img))
  (when (db-has-key? #:db-conn db-conn 'images img-str)
    (define img-obj (make-data-object db-conn image% img-str))
    ; grab all current tags for removal
    (define tag-lst (send img-obj get-tags))
    (remove-img/tags! #:db-conn db-conn img-obj tag-lst)))

; nukes the image from the database in both tables
; adds it back to both tables
; tag-lst assumed to be sorted
(define/contract (db-set! #:db-conn [db-conn sqlc] #:threaded? [threaded? #t] img tag-lst)
  (->* ([or/c string? data-object?]
        [listof string?])
       (#:db-conn connection?
        #:threaded? boolean?)
       (or/c void? thread?))
  (db-purge! #:db-conn db-conn img)
  (add-tags! #:db-conn db-conn img tag-lst)
  (if threaded?
      (thread (λ ()
                (add-tags! #:db-conn db-conn img tag-lst)))
      (add-tags! #:db-conn db-conn img tag-lst)))

; go through each image entry and check if it is a file that still exists
; and then purge from the database if it does not
(define (clean-db! #:db-conn [db-conn sqlc])
  ; grab all the entries in images
  (for ([key (in-table-column 'images 'Path)])
    (unless (file-exists? key) (db-purge! #:db-conn db-conn key))))

; saves only the entries in the list that are duplicates.
; if there are more than two identical entries, they are
; counted more than once, so a final sort and remove-duplicates
; (how ironic) is possibly necessary.
(define (keep-duplicates lst [dups empty])
  (define sorted (sort lst equal?))
  (define len (length sorted))
  (cond [(< len 2) (remove-duplicates dups)]
        [(>= len 2)
         (if (equal? (first sorted) (second sorted))
             (keep-duplicates (rest sorted) (cons (first sorted) dups))
             (keep-duplicates (rest sorted) dups))]))

; search tags table in db for exact matches
; returns a list of paths or empty
(define/contract (search-db-exact #:db-conn [db-conn sqlc] type tag-lst)
  (->* ([or/c 'or 'and]
        [listof string?])
       (#:db-conn connection?)
       (or/c (listof path?) empty?))
  (cond [(zero? (length tag-lst)) empty]
        [else
         ; sql query will complain if a tag as spaces, but no quotes around it
         (define lst-quotes (map ~v tag-lst))
         (define results
           ; loop over the tags we're searching through
           (map (λ (tag-obj)
                  (send tag-obj get-images))
                (select-data-objects db-conn tag% (where (in label lst-quotes)))))
         (define sorted (sort (flatten results) string<?))
         (case type
           ; turn all the strings into paths, remove duplicate items
           [(or) (map string->path (remove-duplicates sorted))]
           ; turn all the strings in paths, keep only duplicate items
           [(and)
            (if (= (length lst-quotes) 1)
                (map string->path (remove-duplicates sorted))
                (map string->path (keep-duplicates sorted)))])]))

; returns a list of paths or empty
(define/contract (search-db-inexact #:db-conn [db-conn sqlc] type tag-lst)
  (->* ([or/c 'or 'and]
        [listof string?])
       (#:db-conn connection?)
       (or/c (listof path?) empty?))
  (cond [(zero? (length tag-lst)) empty]
        [else
         (define search-results
           ; loop over the images in the database
           (for/list ([img-pair (in-table-pairs #:db-conn db-conn 'images)])
             (define tags-searched
               ; loop over the tags supplied
               (for/list ([tag (in-list tag-lst)])
                 (define img-tags (second img-pair))
                 ; list of path-strings and #f
                 (map (λ (img-tag)
                        ; if the string is inside that particular tag
                        (if (string-contains-ci img-tag tag)
                            ; grab its path
                            (first img-pair)
                            #f))
                      img-tags)))
             ; remove any duplicate string-contains-ci matches
             ; for images that have tags containing more than
             ; one of the same phrase (e.g. images with the tags
             ; "beach" "beach towel" will appear more than once)
             (map remove-duplicates tags-searched)))
         ; filter out any #f
         ; list of path only
         (define filtered (sort (filter path? (flatten search-results)) path<?))
         (cond [(or (= (length tag-lst) 1) (eq? type 'or))
                ; remove any duplicates
                (remove-duplicates filtered)]
               [else
                ; keep any duplicates
                (keep-duplicates filtered)])]))

; returns a list of paths or empty
(define/contract (exclude-search-exact #:db-conn [db-conn sqlc] searched-imgs exclusion-tags)
  (->* ([or/c (listof path?) empty?]
        [or/c (listof string?) empty?])
       (#:db-conn connection?)
       (or/c (listof path?) empty?))
  (cond [(or (empty? searched-imgs) (empty? exclusion-tags)) searched-imgs]
        [else
         (define searched-str (if (path? (first searched-imgs))
                                  (map path->string searched-imgs)
                                  searched-imgs))
         (define to-exclude
           (remove-duplicates
            (flatten
             (for/list ([exclusion (in-list exclusion-tags)])
               (cond [(db-has-key? #:db-conn db-conn 'tags exclusion)
                      (define tag-obj (make-data-object db-conn tag% exclusion))
                      (send tag-obj get-images)]
                     [else #f])))))
         (map string->path (remove* (filter string? to-exclude) searched-str))]))

; returns a list of paths or empty
(define/contract (exclude-search-inexact #:db-conn [db-conn sqlc] searched-imgs exclusion-tags)
  (->* ([or/c (listof path?) empty?]
        [or/c (listof string?) empty?])
       (#:db-conn connection?)
       (or/c (listof path?) empty?))
  (cond [(or (empty? searched-imgs) (empty? exclusion-tags)) searched-imgs]
        [else
         (define remove-imgs-messy
           (flatten
            ; loop for each image we've searched
            (for/list ([searched (in-list searched-imgs)])
              (define ex
                (flatten
                 ; loop for each tag we want to exclude
                 (for/list ([exclude (in-list exclusion-tags)])
                   ; go through each of the tags in the searched images for matches
                   ;   with tags we want to exclude
                   ; list of #f and number
                   (define img-obj (make-data-object db-conn image% (path->string searched)))
                   (map (λ (st) (string-contains-ci st exclude)) (send img-obj get-tags)))))
              ; replace each instance of an umber with the path of the image we want to exclude
              (map (λ (te) (if (false? te) te searched)) ex))))
         ; remove #f and duplicates
         (define remove-imgs (remove-duplicates (filter path? remove-imgs-messy)))
         ; finally remove the excluded images from the list of searched images
         (remove* remove-imgs searched-imgs)]))
