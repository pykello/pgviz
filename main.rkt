#lang racket

(require db
         "gui.rkt"
         "memory-view.rkt")

(define (main)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (show-gui)
  (set-debug "Sample debug message")
  (set-attrs `("1" "2" "3" "4") `("v1" "v2" "v3" "v4x"))
  (set-memory-page-cells (list (memory-cell 401 64 "SkyBlue" (λ(x) (0)))
                               (memory-cell 430 23 "Moccasin" (λ(x) (0)))
                               (memory-cell 402 65 "Khaki" (λ(x) (0))))))

(main)
