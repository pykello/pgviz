#lang racket/gui

(provide (all-defined-out))

(define multi-document-panel%
  (class vertical-panel%
    (init-field parent)
    (super-new [parent parent])

    (define choices `("Empty"))

    ;; gui
    (define doc-tabs (new doc-tabs%
                          [parent this]
                          [choices choices]))
    (define container (new vertical-panel%
                           [parent this]))

    ;; state
    (define contents (vector (new message%
                                [parent container]
                                [label "Nothing to show"])))
    (define current-choice 0)

    ;; public functions
    (define/public (set-current-item title content)
      (send doc-tabs set-current-tab title)
      (define current-content (vector-ref contents current-choice))
      (send container delete-child current-content)
      (vector-set! contents current-choice content)
      (send content reparent container))))

(define doc-tabs%
  (class horizontal-panel%

    (init-field parent
                choices
                [active-item 0]
                [callback (λ(idx) (displayln (format "clicked ~a" idx)))])
    (super-new [parent parent]
               [alignment '(left center)]
               [stretchable-height #f]
               [spacing 5]
               [vert-margin 5])

    (new vertical-panel%
         [parent this]
         [stretchable-width #f]
         [stretchable-height #f]
         [min-width 5])

    (define items
      (for/list ([choice choices]
                 [i (in-range 0 100)])
        (new doc-tab-item%
             [parent this]
             [is-active? (eq? i active-item)]
             [on-click (λ() (on-click-item i))]
             [label choice])))

    (new button%
         [parent this]
         [label "New Tab"])

    (define (on-click-item idx)
      (for ([item items]
            [i (in-range 0 100)])
        (send item set-active (eq? i idx)))
      (callback idx))

    (define/public (set-current-tab label)
      (define current-item (list-ref items active-item))
      (send current-item set-label label))
    ))

(define doc-tab-item%
  (class vertical-panel%

    (init-field parent
                label
                [is-active? #f]
                [on-click (λ() (displayln "on-click"))]
                [on-close (λ() (displayln "on-close"))])

    (super-new [parent parent]
               [stretchable-height #f]
               [stretchable-width #f]
               [alignment '(left center)]
               [vert-margin 0]
               [horiz-margin 5])

    (define/public (set-active active?)
      (set! is-active? active?)
      (send underline set-active is-active?)
      (send this refresh))

    (define/override (set-label label)
      (send message set-label label))

    (define label-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(left center)]))
    (define message
      (new message%
         [parent label-panel]
         [label label]
         [vert-margin 7]
         [horiz-margin 5]
         [auto-resize #t]))
    (define close
      (new doc-tab-item-close%
           [parent label-panel]
           [on-click on-close]))
    (define underline
      (new doc-tab-item-underline%
           [parent this]
           [is-active? is-active?]))

    (define/override (on-subwindow-event receiver event)
      (define label-or-close
        (or (eq? receiver label-panel) (eq? receiver close)))
      (cond
           [(and (send event entering?) label-or-close)
            (send underline set-focus)]
           [(and (send event leaving?) label-or-close)
            (send underline remove-focus)]
           [(and (eq? receiver label-panel) (send event button-down? 'left))
            (on-click)]
           [else #f]))))

(define doc-tab-item-underline%
  (class canvas%
    (inherit get-dc)
    (init-field parent
                [is-active? #f]
                [has-focus? #f])

    (super-new [parent parent]
               [min-height 3]
               [style '(transparent)])

    (define/public (set-active active?)
      (set! is-active? active?)
      (send this refresh-now))

    (define/public (set-focus)
      (set! has-focus? #t)
      (send this refresh-now))

    (define/public (remove-focus)
      (set! has-focus? #f)
      (send this refresh-now))

    (define/override (on-paint)
      (define dc (get-dc))
      (define color (if is-active? "orange" "DarkGray"))
      (send dc set-pen color 10 'solid)
      (define draw? (or is-active? has-focus?))
      (when draw?
        (send dc draw-rectangle 0 0 500 10)))))

(define doc-tab-item-close%
  (class horizontal-panel%
    (init-field parent
                [on-click (λ() 0)])

    (super-new [parent parent])

    (new message%
         [parent this]
         [stretchable-height #f]
         [font (make-object font% 12 'default)]
         [label "⊠"])

    (define/override (on-subwindow-event receiver event)
      (define click? (send event button-down? 'left))
      (cond
        [click? (on-click)]
        [else #f]))))
