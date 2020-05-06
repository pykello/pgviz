#lang racket/gui

(require db
         racket/draw
         racket/class
         "gui-components/monitor.rkt"
         "utils.rkt"
         "postgres-gui/heap-viewer.rkt")

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
    [else (define rel (send relation-name get-value))
          (define idx-str (send page-index get-value))
          (define idx (string->number idx-str))
          (with-handlers
              ([exn:fail:sql? show-postgres-error])
            (set-monitor-handler (heap-page-view pgc rel "main" idx set-attrs)))]))

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

(define (set-monitor-handler handler)
  (send monitor set-handler handler))

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
       [label "hostname: "]
       [init-value "localhost"]
       [stretchable-width #f]
       [min-width 200]))

(define postgres-port
  (new text-field%
       [parent postgres-pane]
       [label "port: "]
       [init-value "5432"]
       [stretchable-width #f]
       [min-width 100]))

(define postgres-user
  (new text-field%
       [parent postgres-pane]
       [label "user: "]
       [init-value "hadi"]
       [stretchable-width #f]
       [min-width 150]))

(define postgres-database
  (new text-field%
       [parent postgres-pane]
       [label "database: "]
       [init-value "postgres"]
       [stretchable-width #f]
       [min-width 200]))

(define connect-button
  (new button%
       [parent postgres-pane]
       [label "Connect"]
       [stretchable-width #f]
       [min-width 100]
       [callback on-connect-clicked]))

(define postgres-status
  (new message%
       [label "Not Connected!"]
       [parent postgres-pane]
       [stretchable-width #f]))

(define views-pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]
       [stretchable-height #f]))

(define view-type-pane
  (new choice%
       [parent views-pane]
       [label "Type: "]
       [choices (list "Heap Page")]
       [stretchable-width #f]
       [min-width 200]))

(define relation-name
  (new text-field%
       [parent views-pane]
       [label "Relation: "]
       [init-value "pg_class"]
       [stretchable-width #f]
       [min-width 200]))

(define page-index
  (new text-field%
       [parent views-pane]
       [label "Page Index: "]
       [init-value "0"]
       [stretchable-width #f]
       [min-width 200]))

(define load-view-button
  (new button%
       [parent views-pane]
       [label "Load View"]
       [stretchable-width #f]
       [min-width 100]
       [callback on-load-clicked]))

(define monitor-pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]))

(define monitor
  (new monitor%
       [parent monitor-pane]))

(define list-box-container
  (new group-box-panel%
       [parent monitor-pane]
       [label "Attributes"]
       [horiz-margin 5]
       [stretchable-width #f]
       [min-width 300]
       [vert-margin 5]))

(define list-box
  (new list-box%
       [parent list-box-container]
       [choices empty]
       [label #f]
       [columns `("Name" "Value")]
       [min-height 300]
       [style `(single column-headers clickable-headers)]))

(send list-box set-column-width 0 100 50 200)

(define debug-box
  (new text-field%
       [parent list-box-container]
       [label "Debug"]
       [min-height 100]
       [style `(multiple vertical-label)]))

(main)
