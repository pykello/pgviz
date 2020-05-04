#lang racket/gui

(provide show-gui
         set-debug
         set-attrs
         set-monitor-handler)

(require racket/class
         "gui-components/monitor.rkt"
         "utils.rkt")

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
  (send canvas set-handler handler))

;; gui definition
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
       [stretchable-height #f]
))

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

(define postgres-connect
  (new button%
       [parent postgres-pane]
       [label "connect"]
       [stretchable-width #f]
       [min-width 100]))

(define pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]))

(define canvas
  (new monitor%
       [parent pane]))

(define list-box-container
  (new group-box-panel%
       [parent pane]
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

