#lang racket/gui

(provide (all-defined-out))

(define monitor-handler%
  (class object%
    (define/public (on-left-mouse-click x y)
      (displayln "default left-mouse-click"))

    (define/public (get-view)
      bmp)

    (define/public (get-width)
      1000)

    (define/public (get-height)
      500)

    (define/public (paint dc)
      (send dc draw-text "Default Monitor Handler" 10 10))

    (define bmp (make-object bitmap% 2000 11000))
    (let ([dc (send bmp make-dc)])
      (send dc clear)
      (send this paint dc))
    
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
      (when (and (send event button-changed? 'left)
                 (send event button-down? 'left))
        (define-values (xs ys) (send this get-view-start))
        (let* ([x (send event get-x)]
               [y (send event get-y)])
          (send handler on-left-mouse-click (+ xs x) (+ ys y)))))

    (define/override (on-paint)
      (define bmp (send handler get-view))
      (define dc (get-dc))
      (send dc draw-bitmap bmp 0 0))

    (super-new [style `(hscroll vscroll)])))
