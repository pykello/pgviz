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
  (define header-cells (page-header->memory-cells (get-page-header pgc rel fork idx)))
  (set-memory-page-cells header-cells))

(define (page-header->memory-cells header)
  (for/list ([value (page-header-bytes header)]
             [addr (in-range (page-header-offset header) (page-header-len header))])
    (memory-cell addr value "Medium Goldenrod" (λ (c) (displayln "Header was clicked")))))

(main)
