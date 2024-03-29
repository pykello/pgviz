#lang racket/gui

(require db
         racket/class
         framework
         "gui-components/monitor.rkt"
         "utils.rkt"
         "postgres-gui/heap-viewer.rkt"
         "postgres-gui/btree-viewer.rkt"
         "gui-components/multi-document-panel.rkt")

(define pgc #f)

(define (main)
  (show-gui)
  (set-debug "Sample debug message"))

(define (on-connect-clicked b e)
  (define hostname (send postgres-hostname get-value))
  (define port (send postgres-port get-value))
  (define user (send postgres-user get-value))
  (define database (send postgres-database get-value))
  (with-handlers
      ([exn:fail:sql? (λ (exn)
                        (send postgres-status set-label "Not Connected!"))])
    (when (not (eq? pgc #f))
      (disconnect pgc))
    (set! pgc #f)
    (define conn (postgresql-connect #:user user
                                     #:database database
                                     #:server hostname
                                     #:port (string->number port)))
    (set! pgc conn)
    (send postgres-status set-label "Connected!")))

(define (on-load-clicked b e)
  (cond
    [(eq? pgc #f) (message-box "Error" "Not connected yet")]
    [else (match (send view-type-choice get-selection)
            [0 (load-heap-page)]
            [1 (load-btree-index)]
            [default (displayln "invalid choice")])]))

(define (load-heap-page)
  (let* ([rel (send relation-name get-value)]
         [idx-str (send page-index get-value)]
         [idx (string->number idx-str)])
    (with-handlers
        ([exn:fail:sql? show-postgres-error])
      (define new-contents
        (new monitor%
             [parent window]
             [handler (heap-page-view pgc rel "main" idx set-attrs)]))
      (define title (format "Heap Page: ~a/~a" rel idx))
      (set-current-tab title new-contents))))

(define (load-btree-index)
  (let* ([name (send btree-name get-value)])
    (with-handlers
        ([exn:fail:sql? show-postgres-error])
      (define new-contents
        (new monitor%
             [parent window]
             [handler (btree-view pgc name set-attrs)]))
      (define title (format "B-Tree Index: ~a" name))
      (set-current-tab title new-contents))))

(define (show-postgres-error e)
  (define info (make-hash (exn:fail:sql-info e)))
  (message-box "Error"
               (string-append "ERROR: "
                              (hash-ref info 'message ""))))

;; public interface
(define (show-gui)
  (send window show #t))

(define (set-debug v)
  (send debug-box set-value v))

(define (set-attrs attrs)
  (define names (map (compose ~a first) attrs))
  (define values (map (compose ~a second) attrs))
  (send list-box set names values))

(define (set-current-tab title contents)
  (send multidoc set-current-item title contents))

(define (on-view-type-changed view-type-choice event)
  (define panels (list heap-view-panel btree-view-panel))
  (define selection (send view-type-choice get-selection))
  (send view-attributes-pane change-children
        (λ(lst)
          (list (list-ref panels selection)))))

;; style
(define default-font (make-object font% 12 'default))

;;
;; gui definition
;;
(define window
  (new frame%
       [label "PostgreSQL Visualizer"]
       [width 1800]
       [height 800]
       [style `()]))

(define postgres-pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]
       [stretchable-height #f]))

(define postgres-hostname
  (new text-field%
       [parent postgres-pane]
       [font default-font]
       [label "hostname: "]
       [init-value "localhost"]
       [stretchable-width #f]
       [min-width 250]))

(define postgres-port
  (new text-field%
       [parent postgres-pane]
       [font default-font]
       [label "port: "]
       [init-value "5432"]
       [stretchable-width #f]
       [min-width 100]))

(define postgres-user
  (new text-field%
       [parent postgres-pane]
       [font default-font]
       [label "user: "]
       [init-value "hadi"]
       [stretchable-width #f]
       [min-width 200]))

(define postgres-database
  (new text-field%
       [parent postgres-pane]
       [font default-font]
       [label "database: "]
       [init-value "postgres"]
       [stretchable-width #f]
       [min-width 250]))

(define connect-button
  (new button%
       [parent postgres-pane]
       [font default-font]
       [label "Connect"]
       [stretchable-width #f]
       [min-width 100]
       [callback on-connect-clicked]))

(define postgres-status
  (new message%
       [label "Not Connected!"]
       [font default-font]
       [parent postgres-pane]
       [stretchable-width #f]))

(define views-pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]
       [stretchable-height #f]))

(define view-type-choice
  (new choice%
       [parent views-pane]
       [font default-font]
       [label "Type: "]
       [choices (list "Heap Page" "B-Tree Index")]
       [stretchable-width #f]
       [min-width 200]
       [callback on-view-type-changed]))

(define view-attributes-pane
  (new horizontal-pane%
       [parent views-pane]
       [stretchable-width #f]))

(define heap-view-panel
  (new horizontal-panel%
       [parent view-attributes-pane]
       [stretchable-width #f]))

(define relation-name
  (new text-field%
       [parent heap-view-panel]
       [font default-font]
       [label "Relation: "]
       [init-value "pg_class"]
       [stretchable-width #f]
       [min-width 200]))

(define page-index
  (new text-field%
       [parent heap-view-panel]
       [font default-font]
       [label "Page Index: "]
       [init-value "0"]
       [stretchable-width #f]
       [min-width 200]))

(define btree-view-panel
  (new horizontal-panel%
       [parent view-attributes-pane]
       [stretchable-width #f]
       [style '(deleted)]))

(define btree-name
  (new text-field%
       [parent btree-view-panel]
       [font default-font]
       [label "Index: "]
       [init-value "pg_class_oid_index"]
       [stretchable-width #f]
       [min-width 200]))

(define load-view-button
  (new button%
       [parent views-pane]
       [font default-font]
       [label "Load View"]
       [stretchable-width #f]
       [min-width 100]
       [callback on-load-clicked]))

(define monitor-pane
  (new panel:horizontal-dragable%
       [parent window]
       [horiz-margin 5]
       [spacing 5]
       [style '(border)]))

(define multidoc
  (new multi-document-panel%
       [parent monitor-pane]
       [font default-font]))

(define list-box-container
  (new group-box-panel%
       [parent monitor-pane]
       [font default-font]
       [label "Attributes"]
       [horiz-margin 5]
       [stretchable-width #f]
       [min-width 300]
       [vert-margin 5]))

(define list-box
  (new list-box%
       [parent list-box-container]
       [font default-font]
       [choices empty]
       [label #f]
       [columns `("Name" "Value")]
       [min-height 300]
       [style `(single column-headers clickable-headers)]))

(send list-box set-column-width 0 120 100 200)
(send list-box set-column-width 1 200 100 400)

(define debug-box
  (new text-field%
       [parent list-box-container]
       [font default-font]
       [label "Debug"]
       [min-height 100]
       [style `(multiple vertical-label)]))

(send monitor-pane set-percentages (list 4/5 1/5))

(main)
