#lang racket

(require racket/format
         pict
         db
         db/util/postgresql
         "../utils.rkt"
         "../gui-components/monitor.rkt"
         "../postgres/btree_inspect.rkt")

(provide (all-defined-out))

(define (btree-view pgc name set-attrs)
  (define btree (new btree%
                     [relname name]
                     [pgc pgc]))
                     
  (new btree-view%
       [btree btree]))

(define btree-view%
  (class monitor-handler%

    (init-field btree
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


(define (btree-leaf-pict node [max-visible-items 3])
  (define xmargin 14)
  (define ymargin 16)
  (define items (send node get-items))
  (define item-picts
    (cond
      [(<= (length items) 3)
       (map leaf-item-pict items)]
      [else
       (list (leaf-item-pict (first items))
             (leaf-item-pict (second items))
             (text "⋯")
             (leaf-item-pict (last items)))]))
  (define contents
    (apply hc-append (cons 5 item-picts)))
  (define w (pict-width contents))
  (define h (pict-height contents))
  (define frame
    (filled-rectangle
     (+ w xmargin)
     (+ h ymargin)
     #:color "LightGray"))
  (define framed
    (cc-superimpose frame contents))
  (ct-superimpose (blank (pict-width framed) (+ 20 (pict-height framed)))
                  framed))

(define (leaf-item-pict item)
  (define xmargin 6)
  (define ymargin 12)
  (define v (car item))
  (define v-pict (text v))
  (define w (pict-width v-pict))
  (define h (pict-height v-pict))
  (define frame
    (filled-rounded-rectangle
     (+ w xmargin)
     (+ h ymargin)
     #:color "white"))
  (vc-append (cc-superimpose frame v-pict)
             (pip-arrow-line 0 25 7)))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (define btree (new btree%
                    [relname "t_idx"]
                    [pgc pgc]))
  (define root (send btree get-root))
  (define root-items (send root get-items))
  (define attr-types (send root get-attr-types))
  (btree-leaf-pict root))

(test)



