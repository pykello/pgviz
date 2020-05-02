#lang racket

(require db)
(require "gui.rkt")

(define (main)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (show-gui)
  (set-debug "Sample debug message")
  (set-attrs `("1" "2" "3" "4") `("v1" "v2" "v3" "v4x")))

(main)
