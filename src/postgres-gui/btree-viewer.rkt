#lang racket

(require racket/format
         "../utils.rkt"
         "../gui-components/monitor.rkt")

(provide (all-defined-out))

(define (btree-view pgc name set-attrs)
  (new btree-view%))

(define btree-view%
  (class monitor-handler%

    (init-field [meta `()]
                [root `()]
                [visible-levels 3]
                [max-visible-items 3])

    ;;
    ;; event handler
    ;;
    (define/override (on-left-mouse-click x y)
      0)

    (define/override (paint dc)
      (send dc draw-text "btree viewer" 10 10))

    (define/override (get-width)
      500)

    (define/override (get-height)
      500)

    ;;
    ;; initialize
    ;;
    (super-new)))






