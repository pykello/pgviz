#lang racket/gui

(provide (all-defined-out))

(define monitor-handler%
  (class object%
    (define/public (on-left-mouse-click x y)
      0)

    (define/public (on-right-mouse-click x y)
      0)

    (define/public (get-view)
      (when (null? bmp)
        (define new-bmp (make-object bitmap%
                          (send this get-width)
                          (send this get-height)))
        (let ([dc (send new-bmp make-dc)])
          (send dc clear)
          (send this paint dc))
        (set! bmp new-bmp))
      bmp)

    (define/public (get-width)
      3000)

    (define/public (get-height)
      500)

    (define/public (paint dc)
      (send dc draw-text "Make selections to load a view!" 10 10))

    (define bmp null)
    
    (super-new)))

(define monitor%
  (class canvas%
    (inherit get-dc get-width get-height refresh-now)
    (define handler (new monitor-handler%))

    (define/public (set-handler new-handler)
      (set! handler new-handler)
      (define width (send handler get-width))
      (define height (send handler get-height))
      (send this init-auto-scrollbars width height 0 0))

    (define/override (on-event event)
      (define (button-click? which)
        (and (send event button-changed? which)
             (send event button-down? which)))
      (define-values (xs ys) (send this get-view-start))
      (let* ([x (send event get-x)]
             [y (send event get-y)])
        (when (button-click? 'left)
          (send handler on-left-mouse-click (+ xs x) (+ ys y)))
        (when (button-click? 'right)
          (send handler on-right-mouse-click (+ xs x) (+ ys y)))))

    (define/override (on-paint)
      (define bmp (send handler get-view))
      (define dc (get-dc))
      (send dc draw-bitmap bmp 0 0))

    (super-new [style `(hscroll vscroll)])))
