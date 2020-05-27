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

(struct btree-assoc (type value pict))

(define btree-view%
  (class monitor-handler%

    (init-field btree
                [visible-levels 3]
                [max-visible-items 3])

    ;;
    ;; event handler
    ;;
    (define/override (on-left-mouse-click x y)
      (define item (findf (assoc-contains? root-pict x y) assocs))
      (when (not (eq? #f item))
        (displayln (btree-assoc-type item))))
        

    (define/override (paint dc)
      (send dc set-smoothing 'aligned)
      (draw-pict root-pict dc 0 0))
      
    (define/override (get-width)
      (exact-round (pict-width root-pict)))

    (define/override (get-height)
      (exact-round (pict-height root-pict)))

    (define-values (root-pict assocs)
      (btree-pict btree))

    ;;
    ;; initialize
    ;;
    (super-new)))

(define (assoc-contains? base x y)
  (λ (assoc)
    (define p (btree-assoc-pict assoc))
    (define-values (px py) (lt-find base p))
    (define w (pict-width p))
    (define h (pict-height p))
    (and (<= px x (+ px w))
         (<= py y (+ py h)))))

(define (btree-pict btree)
  (define metapage-text (inset (text "Metapage") 20 7))
  (define metapage-pict (frame-items (list metapage-text)))
  (define-values (subtree-pict assocs)
    (btree-child-pict (send btree get-root)))
  (define root-pict (btree-assoc-pict (first assocs)))
  (define combined (aligned-v-append 30 metapage-pict subtree-pict metapage-pict root-pict))
  (define with-arrow (add-child-arrows combined (list metapage-pict) (list root-pict)))
  (define padded (inset with-arrow 50))
  (values padded (cons (btree-assoc 'metapage btree metapage-pict) assocs)))

;;
;; returns (values pict (list node-assoc))
;;
(define (btree-child-pict child)
  (cond
    [(is-a? child btree-node%) (btree-node-pict child)]
    [else (define root-pict (tuple-pointer-pict child))
          (values root-pict (list (btree-assoc 'tuple-pointer child root-pict)))]))

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
            (list (btree-assoc-pict (first ptr-assocs)))))
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
        (list (cc-superimpose (blank 20 child-root-height)
                              (text spacer-text)))))
  (define child-picts
    (append left-child-picts
            child-spacer-picts
            right-child-picts))

  ;; combined pict, without arrows
  (define child-spacing (if leaf? 6 25))
  (define children-combined (apply ht-append (cons child-spacing child-picts)))
  (define-values
    (aligned-child aligned-item)
    (if (< (length left-child-root-picts) 2)
        (values children-combined root-pict)
        (values (second left-child-root-picts) (second left-item-picts))))
  (define combined-pict
    (aligned-v-append 50 root-pict children-combined aligned-item aligned-child))

  (define with-child-arrows
    (add-child-arrows combined-pict
                      (append left-item-picts right-item-picts)
                      (append left-child-root-picts right-child-root-picts)))

  (define with-sibling-arrows
    (if (eq? (send node get-type) 'leaf)
        with-child-arrows
        (add-sibling-arrows with-child-arrows (append left-child-root-picts
                                                      child-spacer-picts
                                                      right-child-root-picts))))

  (define assocs
    (append (list (btree-assoc 'node node root-pict))
            left-child-assocs
            right-child-assocs))

  (values with-sibling-arrows assocs))

(define (add-child-arrows combined item-picts child-picts)
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

(define (aligned-v-append d p1 p2 p1-align p2-align)
  (define-values (cx1 cy1) (cc-find p1 p1-align))
  (define-values (cx2 cy2) (cc-find p2 p2-align))
  (define p1x (- cx2 cx1))
  (define p2-adjusted
    (cond
      [(>= p1x 0) p2]
      [else (pad-left p2 (- p1x))]))
  (define p1-adjusted
    (cond
      [(>= p1x 0) (pad-left p1 p1x)]
      [else p1]))
  (vl-append d p1-adjusted p2-adjusted))

;;
;; pads p by some empty space on the left defined by padding
;;
(define (pad-left p padding)
  (rc-superimpose (blank (+ (pict-width p) padding)
                         (pict-height p))
                  p))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (define btree (new btree%
                     [relname "tx_idx"]
                     [pgc pgc]))
  (define-values (p assocs) (btree-pict btree))
  assocs)

;;(test)



