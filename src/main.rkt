#lang racket

(require db
         racket/draw
         "gui.rkt"
         "postgres-gui/heap-viewer.rkt")

(define (main)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (show-gui)
  (set-debug "Sample debug message")
  (set-attrs `(("name 1" "value 1") ("name 2" "value 2")))
  (set-monitor-handler (heap-page-view pgc "pg_class" "main" 0 set-attrs)))

(main)
