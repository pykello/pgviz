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

(define (btree-child-pict child)
  (cond
    [(is-a? child btree-node%) (btree-node-pict child)]
    [else (tuple-pointer-pict child)]))

(define (btree-node-pict node [max-visible-items 3])
  (define items (send node get-items))
  (define-values (high-key valid-items)
    (match (send node has-high-key)
      [#t (values (list (car items)) (cdr items))]
      [#f (values (list) items)]))
  (define items-to-show
    (cond
      [(<= (length items) 3) items]
      [else
       (list (first items) (second items) "⋯" (last items))]))
  (define child-picts
    (for/list ([item items-to-show])
      (cond
        [(string? item) (inset (text " ⋯⋯ ") 0 10)]
        [else (btree-child-pict (cdr item))])))
  (define item-picts
    (for/list ([item items-to-show])
      (cond
        [(string? item) (text item)]
        [else (node-item-pict item)])))
  (define high-key-pict
    (map node-item-pict high-key))
  (define root-pict
    (frame-items (append high-key-pict item-picts)))
  (define all-nodes
    (vc-append 50
               root-pict
               (apply ht-append (cons 25 child-picts))))
  (define with-child-pointers
    (for/fold ([combined all-nodes])
              ([item-pict item-picts]
               [child-pict child-picts]
               [item items-to-show])
      (cond
        [(string? item) (values combined)]
        [else (values (pin-arrow-line 7 combined
                                      item-pict cb-find
                                      child-pict ct-find))])))
  (define leaf? (eq? (send node get-type) 'leaf))
  (define with-sibling-pointers
    (if leaf?
        with-child-pointers
        (for/fold ([combined with-child-pointers])
                  ([from child-picts]
                   [to (cdr child-picts)])
          (values (pin-arrows-line 7 combined
                                  from rt-find
                                  to lt-find)))))
  with-sibling-pointers)

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
  (define xmargin 6)
  (define ymargin 12)
  (define v-pict (text v))
  (define w (pict-width v-pict))
  (define h (pict-height v-pict))
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
                    [relname "t_idx"]
                    [pgc pgc]))
  (define root (send btree get-root))
  (define root-items (send root get-items))
  (define attr-types (send root get-attr-types))
  (btree-node-pict root))

(test)



