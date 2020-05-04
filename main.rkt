#lang racket

(require db
         "gui.rkt"
         "memory-view.rkt"
         "pageinspect.rkt")

(define (main)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (show-gui)
  (set-debug "Sample debug message")
  (set-attrs `("1" "2" "3" "4") `("v1" "v2" "v3" "v4x"))
  (set-memory-page-cells (list (memory-cell 401 64 "SkyBlue" (λ(x) (0)))
                               (memory-cell 430 23 "Moccasin" (λ(x) (0)))
                               (memory-cell 402 65 "Khaki" (λ(x) (0)))))
  (load-heap-page pgc "t" "main" 0))

(define (load-heap-page pgc rel fork idx)
  (define page-header (get-page-header pgc rel fork idx))
  (set-memory-page-size (page-header-pagesize page-header))
  (define header-cells (page-header->memory-cells page-header))
  (define tuple-cells (htups->memory-cells (get-heap-tuples pgc rel fork idx)))
  (set-memory-page-cells (append header-cells tuple-cells)))

(define (page-header->memory-cells header)
  (for/list ([value (page-header-bytes header)]
             [addr (in-range (page-header-offset header) (page-header-len header))])
    (memory-cell addr value "Medium Goldenrod" (λ (c) (displayln "Header was clicked")))))

(define (htups->memory-cells htups)
  (flatten
   (for/list ([tup htups])
     (define itemid (heap-tuple-itemid tup))
     (define itemid-from (item-id-offset itemid))
     (define itemid-to (+ itemid-from (item-id-len itemid)))
     (define itemid-cells (bytes->cells (item-id-bytes itemid)
                                        itemid-from itemid-to
                                        "DarkKhaki"
                                        (λ (c) (displayln "ItemId was clicked"))))
     (define header (heap-tuple-header tup))
     (define tuple-from (htup-header-offset header))
     (define tuple-to (+ tuple-from (htup-header-len header)))
     (define tuple-cells (bytes->cells (htup-header-bytes header)
                                       tuple-from tuple-to
                                       "NavajoWhite"
                                       (λ (c) (displayln "Tuple was clicked"))))
     (append itemid-cells tuple-cells))))

(define (bytes->cells bytes from to color callback) 
  (for/list ([value bytes]
             [addr (in-range from to)])
    (memory-cell addr value color callback)))

(main)
