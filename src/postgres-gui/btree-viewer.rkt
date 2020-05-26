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
  (define leaf? (eq? (send node get-type) 'leaf))
  (define-values (high-key valid-items)
    (match (send node has-high-key)
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

  ;; build picts to show as children
  (define left-child-picts
    (map (compose btree-child-pict cdr) left-items))
  (define right-child-picts
    (map (compose btree-child-pict cdr) right-items))
  (define child-spacer-picts
    (if (null? right-child-picts)
        `()
        (list (inset (text (if leaf? "⋯" " ⋯⋯ ")) 0 10))))
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

  with-child-arrows)

(define (add-item2child-arrows combined item-picts child-picts)
  (for/fold ([agg combined])
            ([item-pict item-picts]
             [child-pict child-picts])
    (values (pin-arrow-line 7 agg
                            item-pict cb-find
                            child-pict ct-find))))

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



