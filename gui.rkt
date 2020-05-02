#lang racket/gui

(provide show-gui
         set-debug
         set-attrs)

(require racket/class)

;; public interface
(define (show-gui)
  (send window show #t))

(define (set-debug v)
  (send debug-box set-value v))

(define (set-attrs . v)
  (send/apply list-box set v))

;; gui definition
(define window
  (new frame%
       [label "Icon Viewer"]
       [width 800]
       [height 600]
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

(define canvas
  (new canvas%
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

