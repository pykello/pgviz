#lang racket/gui

(provide show-gui
         set-debug
         set-attrs
         set-monitor-handler)

(require racket/class
         "memory-view.rkt"
         "monitor.rkt")

;; public interface
(define (show-gui)
  (send window show #t))

(define (set-debug v)
  (send debug-box set-value v))

(define (set-attrs . v)
  (send/apply list-box set v))

(define (set-monitor-handler handler)
  (send canvas set-handler handler))

;; gui definition
(define window
  (new frame%
       [label "PostgreSQL Visualizer"]
       [width 1800]
       [height 800]
       [style `()]))

(define search-box
  (new text-field%
       [parent window]
       [label "Search: "]))

(define pane
  (new horizontal-pane%
       [parent window]
       [horiz-margin 5]
       [spacing 5]))

;;(define canvas
;;  (new memory-view%
;;       [parent pane]
;;       [cells (list (memory-cell 400 65 "SkyBlue" (λ(x) (0))))]))

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

(define debug-box
  (new text-field%
       [parent list-box-container]
       [label "Debug"]
       [min-height 100]
       [style `(multiple vertical-label)]))

