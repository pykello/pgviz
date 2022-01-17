#lang racket/gui

(provide (all-defined-out))

(require racket/format
         "../utils.rkt")

;;
;; tab bar + contents container. Each tab is associated with a content.
;; To set title and current of current tab, users can use set-current-item.
;;
(define multi-document-panel%
  (class vertical-panel%
    (init-field parent)
    (init-field font)
    (super-new [parent parent])

    ;; public functions
    (define/public (set-current-item title content)
      (send tab-bar set-current-tab title)
      (set! contents (list-set contents current-choice content))
      (on-change-tab current-choice))

    ;; event handlers
    (define (on-new-tab idx)
      (set! current-choice idx)
      (define label "New Tab")
      (set! contents (append contents (list (placeholder))))
      (set! choices (append choices (list label))))

    (define (on-change-tab idx)
      (set! current-choice idx)
      (define content (list-ref contents idx))
      (send content reparent container)
      (send container change-children (λ(a) (list)))
      (send container add-child content))

    (define (on-close-tab idx)
      (set! contents (delete-at contents idx))
      (set! choices (delete-at choices idx)))

    (define (placeholder)
      (new message%
           [parent container]
           [font font]
           [label "Nothing to show"]))

    (define choices `("New Tab"))

    ;; gui
    (define tab-bar (new tab-bar%
                          [parent this]
                          [choices choices]
                          [font font]
                          [on-new-tab on-new-tab]
                          [on-change-tab on-change-tab]
                          [on-close-tab on-close-tab]))
    (define container (new vertical-panel%
                           [parent this]))

    ;; state
    (define contents (list (placeholder)))
    (define current-choice 0)))

;;
;; contains multiple tabs + "New Tab" button.
;;
(define tab-bar%
  (class horizontal-panel%

    (init-field parent
                choices
                font
                [active-item 0]
                [on-change-tab (λ(idx) (displayln (format "clicked ~a" idx)))]
                [on-new-tab (λ(idx) (displayln "default new tab"))]
                [on-close-tab (λ(idx) (displayln (format "closed ~a" idx)))])

    ;; public methods
    (define/public (set-current-tab label)
      (define current-item (list-ref items active-item))
      (send current-item set-label label))

    ;; construct parent
    (super-new [parent parent]
               [alignment '(left center)]
               [stretchable-height #f]
               [spacing 5]
               [vert-margin 5])

    ;;
    ;; state
    ;;
    (define items `())

    ;;
    ;; initialize
    ;;
    (define (initialize)
      (for ([choice choices])
        (add-tab choice)))

    ;;
    ;; private functions
    ;;
    
    (define (add-tab label)
      (define idx (length items))
      (define item
        (new tab-item%
             [parent item-container]
             [is-active? #f]
             [font font]
             [on-click (λ() (on-click-item item))]
             [on-close (λ() (on-close-item item))]
             [label label]))
      (set! items (append items (list item)))
      (set-active-item idx)
      idx)

    (define (set-active-item idx)
      (for ([item items]
            [i (in-range 0 100)])
        (send item set-active (eq? i idx)))
      (set! active-item idx))

    (define (on-click-item item)
      (define idx (index-of items item))
      (set-active-item idx)
      (on-change-tab idx))

    (define (on-close-item item-to-close)
      (define item-to-close-idx (index-of items item-to-close))
      (define updated-tab-count
        (max 1 (- (length items) 1)))
      (define updated-active-idx
        (cond
          [(>= item-to-close-idx active-item) (min active-item (- updated-tab-count 1))]
          [else (- active-item 1)]))
      (send item-container delete-child item-to-close)
      (set! items (delete-at items item-to-close-idx))
      (when (null? items)
        (on-new-tab-clicked #f #f))
      (on-close-tab item-to-close-idx)
      (set-active-item updated-active-idx)
      (on-change-tab updated-active-idx))

    (define (on-new-tab-clicked self evt)
      (define idx (add-tab "New Tab"))
      (on-new-tab idx)
      (on-change-tab idx))

    ;;
    ;; gui
    ;;
    (define item-container
      (new horizontal-panel%
         [parent this]
         [stretchable-width #f]
         [stretchable-height #f]
         [min-width 5]))

    (new button%
         [parent this]
         [label "New Tab"]
         [font font]
         [callback on-new-tab-clicked])

    (initialize)))

;;
;; Single tab item
;;
(define tab-item%
  (class vertical-panel%

    (init-field parent
                label
                font
                [is-active? #f]
                [on-click (λ() (displayln "on-click"))]
                [on-close (λ() (displayln "on-close"))])

    (super-new [parent parent]
               [stretchable-height #f]
               [stretchable-width #f]
               [alignment '(left center)]
               [vert-margin 0]
               [horiz-margin 5])

    ;;
    ;; public methods
    ;;
    (define/public (set-active active?)
      (set! is-active? active?)
      (send underline set-active is-active?)
      (send this refresh))

    (define/override (set-label label)
      (send message set-label label))

    ;;
    ;; gui
    ;;
    (define label-panel
      (new horizontal-panel%
           [parent this]
           [alignment '(left center)]))
    (define message
      (new message%
         [parent label-panel]
         [label label]
         [font font]
         [vert-margin 7]
         [horiz-margin 5]
         [auto-resize #t]))
    (define close
      (new tab-item-close%
           [parent label-panel]
           [on-click on-close]))
    (define underline
      (new tab-item-underline%
           [parent this]
           [is-active? is-active?]))

    ;;
    ;; handle mouse events
    ;;
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

;;
;; underline decoration for a single tab item
;;
(define tab-item-underline%
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

;;
;; close button on a tab item
;;
(define tab-item-close%
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
