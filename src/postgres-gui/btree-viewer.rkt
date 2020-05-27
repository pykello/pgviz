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

(define (btree-internal-pict node [visible-levels 3] [max-visible-items 3])
  0)

;;
;; returns (values pict (list node-assoc))
;;
(define (btree-child-pict child)
  (cond
    [(is-a? child btree-node%) (btree-node-pict child)]
    [else (values (tuple-pointer-pict child) `())]))


(define (btree-node-pict node [max-visible-items 3])
  (define items (send node get-items))
  (define leaf? (eq? (send node get-type) 'leaf))
  (define-values (high-key valid-items)
    (match (send node has-high-key?)
      [#t (values (list (car items)) (cdr items))]
      [#f (values (list) items)]))
  (define-values (left-items right-items)
    (cond
      [(<= (length valid-items) max-visible-items) (values valid-items `())]
      [else (values (take valid-items (- max-visible-items 1))
                    (list (last valid-items)))]))

  ;; build picts to show in root
  (define high-key-picts
    (map node-item-pict high-key))
  (define left-item-picts
    (map node-item-pict left-items))
  (define right-item-picts
    (map node-item-pict right-items))
  (define item-spacer-picts
    (if (null? right-item-picts)
        `()
        (list (text "⋯"))))
  (define item-picts
    (append high-key-picts
            left-item-picts
            item-spacer-picts
            right-item-picts))

    ;; root
  (define root-pict
    (frame-items item-picts))

  ;;
  ;; aggregates btree-child-pict results of children as
  ;; (values root-picts assocs child-picts)
  ;; child-picts are the complete child subtree. root-picts
  ;; are pict of those subtrees.
  ;;
  (define (agg-children child-items)
    (for/fold ([roots `()]
               [assocs `()]
               [picts `()])
              ([item child-items])
      (define ptr (cdr item))
      (define-values (ptr-pict ptr-assocs)
        (btree-child-pict ptr))
      (define ptr-root
        (if (null? ptr-assocs)
            `()
            (list (cdr (first ptr-assocs)))))
      (values (append roots ptr-root)
              (append assocs ptr-assocs)
              (append picts (list ptr-pict)))))

  ;; build picts to show as children
  (define-values (left-child-root-picts left-child-assocs left-child-picts)
    (agg-children left-items))
  (define-values (right-child-root-picts right-child-assocs right-child-picts)
    (agg-children right-items))

  (define child-root-height
    (if (null? left-child-root-picts)
        30
        (pict-height (first left-child-root-picts))))

  (define spacer-text (if leaf? "⋯" " ⋯⋯ "))

  (define child-spacer-picts
    (if (null? right-child-picts)
        `()
        (list (cc-superimpose (blank 50 child-root-height)
                              (text spacer-text)))))
  (define child-picts
    (append left-child-picts
            child-spacer-picts
            right-child-picts))

  ;; combined pict, without arrows
  (define child-spacing (if leaf? 9 25))
  (define combined-pict
    (vc-append 50
               root-pict
               (apply ht-append (cons child-spacing child-picts))))

  (define with-child-arrows
    (add-item2child-arrows combined-pict
                           (append left-item-picts right-item-picts)
                           (append left-child-picts right-child-picts)))

  (define with-sibling-arrows
    (add-sibling-arrows with-child-arrows (append left-child-root-picts
                                                  child-spacer-picts
                                                  right-child-root-picts)))

  (define assocs
    (append (list (cons node root-pict))
            left-child-root-picts
            right-child-root-picts))

  (values with-sibling-arrows assocs))

(define (add-item2child-arrows combined item-picts child-picts)
  (for/fold ([agg combined])
            ([item-pict item-picts]
             [child-pict child-picts])
    (values (pin-arrow-line 7 agg
                            item-pict cb-find
                            child-pict ct-find))))

(define (add-sibling-arrows base node-picts)
  (define to-list
    (if (null? node-picts)
        `()
        (cdr node-picts)))
  (for/fold ([agg base])
            ([from node-picts]
             [to to-list])
    (values (pin-arrows-line 7 agg
                             from rc-find
                             to lc-find))))

(define (frame-items item-picts)
  (define xmargin 14)
  (define ymargin 16)
  (define contents
    (apply hc-append (cons 5 item-picts)))
  (define w (pict-width contents))
  (define h (pict-height contents))
  (define frame
    (filled-rectangle
     (+ w xmargin)
     (+ h ymargin)
     #:color "LightGray"))
  (cc-superimpose frame contents))

(define (node-item-pict v)
  (round-framed-text (car v)))

(define (round-framed-text v)
  (define xmargin 8)
  (define ymargin 12)
  (define sz (if (eq? v "-∞")
                 16
                 12))
  (define v-pict (text v null sz))
  (define w (pict-width v-pict))
  (define h 16)
  (define frame
    (filled-rounded-rectangle
     (+ w xmargin)
     (+ h ymargin)
     #:color "white"))
  (inset (cc-superimpose frame v-pict) 1))


(define (tuple-pointer-pict tid)
  (define txt
    (format "~a,~a" (first tid) (second tid)))
  (round-framed-text txt))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (define btree (new btree%
                    [relname "tx_idx"]
                    [pgc pgc]))
  (define root (send btree get-root))
  (define root-items (send root get-items))
  (define attr-types (send root get-attr-types))
  (define-values (p assocs) (btree-node-pict root))
  p)

(test)



