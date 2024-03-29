#lang racket/gui

(require racket/format
         "../utils.rkt"
         "monitor.rkt")

(provide (all-defined-out))

(struct memory-cell (address
                     value
                     color
                     callback))

(struct memory-layout (visible-rows ;; (list y row-offset)
                       spacers ;; (list y)
                       cell-positions ;; (list (x y memory-cell))
                       ))


(define memory-view%
  (class monitor-handler%
    ;;
    ;; fields
    ;;
    (init-field [cells `()]
                [memory-size 8192]
                [per-row 32]
                [legend legend]
                [font-size 12])

    ;;
    ;; constants
    ;;
    (define cell-side 40)
    (define hmargin 160)
    (define vmargin 100)

    ;;
    ;; variables
    ;;
    (define layout #f)

    ;;
    ;; event handler
    ;;
    (define/override (on-left-mouse-click x y)
      (let* ([point-in-cell? (λ (c) (and (<= (first c) x (+ (first c) cell-side))
                                           (<= (second c) y (+ (second c) cell-side))))]
               [cell (findf point-in-cell? (memory-layout-cell-positions layout))])
          (when (list? cell)
            (let ([callback (memory-cell-callback (third cell))])
              (callback)))))

    (define/override (paint dc)
      (when (eq? layout #f)
        (set! layout (get-memory-layout)))
      (draw-layout dc layout)
      (draw-legend dc legend))

    (define/override (get-width)
      (when (eq? layout #f)
        (set! layout (get-memory-layout)))
      (+ (* 2 hmargin) (* per-row cell-side)))

    (define/override (get-height)
      (when (eq? layout #f)
        (set! layout (get-memory-layout)))
      (let* ([visible-row-count (length (memory-layout-visible-rows layout))]
             [spacer-count (length (memory-layout-spacers layout))])
        (+ ( * 2 vmargin) (* (+ visible-row-count spacer-count) cell-side))))

    ;;
    ;; private methods
    ;;
    (define (draw-legend dc legend)
      (for ([item legend]
            [i (in-range 40)])
        (define brush (first item))
        (define label (second item))
        (define x (+ 50 (* 200 (quotient i 2))))
        (define y (+ 20 (* 30 (remainder i 2))))
        (send dc set-brush (->brush brush))
        (send dc draw-rectangle x y 20 20)
        (send dc draw-text label (+ x 25) y)))

    (define (draw-layout dc layout)
      (send dc set-brush "black" 'transparent)
      (send dc set-font (make-object font% font-size 'modern))
      (define label-width (log memory-size 16))
      ;; draw rows
      (for ([visible-row (memory-layout-visible-rows layout)])
        (define y (first visible-row))
        (define row (second visible-row))
        (for ([col (in-range per-row)])
          (define x (+ hmargin (* col cell-side)))
          (send dc draw-rectangle x y (+ 1 cell-side) (+ 1 cell-side)))
        (define label (format "~a-~a"
                              (hex-format (first-addr row) label-width)
                              (hex-format (last-addr row) label-width)))
        (define-values (w h s1 s2) (send dc get-text-extent label))
        (define label-x (- hmargin w 10))
        (define label-y (+ y (/ cell-side 2) (* h -0.5)))
        (send dc draw-text label label-x label-y))
      ;; draw spacers
      (for ([y (memory-layout-spacers layout)])
        (send dc draw-rectangle hmargin y (+ 1 (* cell-side per-row)) (+ 1 cell-side))
        (for ([i (in-range -2 3)])
          (define x (+ vmargin (* per-row cell-side 0.5) (* i cell-side)))
          (send dc set-brush "black" 'solid)
          (send dc draw-ellipse x (+ y (/ cell-side 2)) 4 4)
          (send dc set-brush "black" 'transparent)))
      ;; draw cells
      (for ([cell-pos (memory-layout-cell-positions layout)])
        (define x (first cell-pos))
        (define y (second cell-pos))
        (define cell (third cell-pos))
        (send dc set-brush (->brush (memory-cell-color cell)))
        (send dc draw-rectangle x y (+ 1 cell-side) (+ 1 cell-side))
        (define label (hex-format (memory-cell-value cell) 2 ""))
        (define-values (w h s1 s2) (send dc get-text-extent label))
        (define label-x (+ x (/ cell-side 2) (* -0.5 w)))
        (define label-y (+ y (/ cell-side 2) (* -0.5 h)))
        (send dc draw-text label label-x label-y)))

    (define (get-memory-layout)
      (define rows (ceiling (/ memory-size per-row)))
      (define-values (visible-rows spacers)
        (for/fold ([visible-rows-agg `()]
                   [spacers-agg `()]
                   [y vmargin]
                   [prev-visible? #f]
                   #:result (values visible-rows-agg spacers-agg))
                  ([row (in-range rows)])
          (define visible?
            (or (eq? row 0)
                (eq? row (- rows 1))
                (memf (λ (c) (cell-in-row c row)) cells)))
          (cond
            [visible? (values (cons (list y row) visible-rows-agg)
                              spacers-agg
                              (+ y cell-side)
                              #t)]
            [prev-visible? (values visible-rows-agg
                                   (cons y spacers-agg)
                                   (+ y cell-side)
                                   #f)]
            [else (values visible-rows-agg spacers-agg y #f)])))
      (define cell-positions
        (for/list ([cell cells])
          (define addr (memory-cell-address cell))
          (define visible-row (findf (λ (r) (cell-in-row cell (second r))) visible-rows))
          (define x (+ hmargin (* cell-side (remainder addr per-row))))
          (define y (first visible-row))
          (list x y cell)))
      (memory-layout visible-rows spacers cell-positions))

    (define (first-addr row)
      (* row per-row))

    (define (last-addr row)
      (- (* (+ 1 row) per-row) 1))

    (define (cell-in-row cell row)
      (<= (first-addr row)
          (memory-cell-address cell)
          (last-addr row)))

    (define (->brush v)
      (if (string? v) (make-object brush% v) v))

    ;;
    ;; initialize
    ;;
    (super-new)))
