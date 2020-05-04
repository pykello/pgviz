#lang racket/gui

(provide heap-page-view)

(require "../gui-components/memory-view.rkt"
         "../gui-components/monitor.rkt"
         "../postgres/pageinspect.rkt")

(define header-brush "Medium Goldenrod")
(define itemid-brush1 "LightSkyBlue")
(define itemid-brush2 "Medium Turquoise")
(define htup-brush "NavajoWhite")
(define htup-header-brush (make-object brush% "NavajoWhite" 'bdiagonal-hatch))

(define (heap-page-view pgc rel fork idx)
  (define legend `((,header-brush "Page Header")
                            (,itemid-brush1 "ItemId")
                            (,itemid-brush2 "ItemId")
                            (,htup-brush "Heap Tuple Data")
                            (,htup-header-brush "Heap Tuple Header")))

  (define page-header (get-page-header pgc rel fork idx))
  (define page-size (page-header-pagesize page-header))
  (define header-cells (page-header->memory-cells page-header))
  (define tuple-cells (htups->memory-cells (get-heap-tuples pgc rel fork idx)))
  (define cells (append header-cells tuple-cells))
  (new memory-view%
       [cells cells]
       [legend legend]
       [memory-size page-size]))

(define (page-header->memory-cells header)
  (for/list ([value (page-header-bytes header)]
             [addr (in-range (page-header-offset header) (page-header-len header))])
    (memory-cell addr value header-brush (λ (c) (displayln "Header was clicked")))))

(define (htups->memory-cells htups)
  (flatten
   (for/list ([tup htups]
              [idx (in-range 1024)])
     (define itemid-color (if (odd? idx) itemid-brush1 itemid-brush2))
     (define itemid (heap-tuple-itemid tup))
     (define itemid-from (item-id-offset itemid))
     (define itemid-to (+ itemid-from (item-id-len itemid)))
     (define itemid-cells (bytes->cells (item-id-bytes itemid)
                                        itemid-from itemid-to
                                        itemid-color
                                        (λ (c) (displayln "ItemId was clicked"))))
     (define header (heap-tuple-header tup))
     (define header-cells
       (cond
         [(eq? header #f) `()]
         [else (bytes->cells (htup-header-bytes header)
                             (htup-header-offset header)
                             (htup-header-len header)
                             htup-header-brush
                             (λ (c) (displayln "Tuple header was clicked")))]))
     (define data-cells (bytes->cells (heap-tuple-data-bytes tup)
                                      (heap-tuple-data-offset tup)
                                      (heap-tuple-data-len tup)
                                      htup-brush
                                      (λ (c) (displayln "Tuple was clicked"))))
     (append itemid-cells header-cells data-cells))))

(define (bytes->cells bytes from len color callback)
  (define to (+ from len))
  (for/list ([value bytes]
             [addr (in-range from to)])
    (memory-cell addr value color callback)))