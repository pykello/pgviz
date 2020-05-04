#lang racket

(require db
         racket/draw
         "gui.rkt"
         "gui-components/memory-view.rkt"
         "gui-components/monitor.rkt"
         "postgres/pageinspect.rkt"
         "postgres-gui/heap-viewer.rkt")

(define (main)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (show-gui)
  (set-debug "Sample debug message")
  (set-attrs `("1" "2" "3" "4") `("v1" "v2" "v3" "v4x"))
  (set-monitor-handler (heap-page-view pgc "pg_class" "main" 0)))

(main)
