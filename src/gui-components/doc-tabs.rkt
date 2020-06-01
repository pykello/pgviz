#lang racket/gui

(provide (all-defined-out))

(define doc-tabs%
  (class horizontal-panel%

    (init-field parent
                choices
                [callback (λ(idx) 0)])
    (super-new [parent parent]
               [alignment '(left center)]
               [stretchable-height #f]
               [spacing 5])

    (define items
      (for/list ([choice choices])
        (new doc-tab-item%
             [parent this]
             [label choice])))
    ))

(define doc-tab-item%
  (class vertical-panel%

    (init-field parent
                label
                [on-click (λ() 0)]
                [on-close (λ() 0)])

    (super-new [parent parent]
               [stretchable-height #f]
               [stretchable-width #f]
               [alignment '(left center)])

    (define label-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(left center)]))
    (define message
      (new message%
         (parent label-panel)
         (label label)))
    (define close
      (new message%
           [parent label-panel]
           [stretchable-height #f]
           [font (make-object font% 12 'default)]
           [label "⊠"]))
    (displayln (send message get-height))
    (displayln (send close get-height))
    (define underline
      (new canvas%
           [parent this]
           [min-height 3]
           [paint-callback (λ(self dc)
                             (send dc set-pen "orange" 10 'solid)
                             (send dc draw-rectangle 0 0 500 10))]))

    ))
