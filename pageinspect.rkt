#lang racket

(require db)

(provide (all-defined-out))

;; data structures

(struct page-header (lsn
                     checksum
                     flags
                     lower
                     upper
                     special
                     pagesize
                     version
                     prune_xid))

;; exported functions

(define (page-exists? pgc relname fork idx)
  (with-handlers ([exn:fail:sql? (λ (exn) #f)])
    (get-raw-page pgc relname fork idx)
    #t))

(define (relation-page-count pgc relname fork)
  (define last-page-idx
    (upperbound (λ (v) (page-exists? pgc relname fork v))
                0 134217728 0))
  (+ 1 last-page-idx))

(define (get-raw-page pgc relname fork idx)
  (query-value pgc
               "SELECT * FROM get_raw_page($1, $2, $3)"
               relname fork idx))

(define (get-page-header pgc relname fork idx)
  (define result
    (query-row pgc
               "SELECT lsn::text,
                       checksum,
                       flags,
                       lower,
                       upper,
                       special,
                       pagesize,
                       version,
                       prune_xid::text
                FROM page_header(get_raw_page($1, $2, $3))"
               relname fork idx))
  (apply page-header (vector->list result)))

;; Finds maximum value v in [min, max] which (satisifies? v)
;; If not found, default is returned.
(define (upperbound satisfies? min max default)
  (define (bs min max best)
    (define mid (quotient (+ min max) 2))
    (cond
      [(> min max) best]
      [(satisfies? mid) (bs (+ mid 1) max mid)]
      [else (bs min (- mid 1) best)]))
  (bs min max default))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (displayln (bytes-length (get-raw-page pgc "t" "main" 0)))
  (displayln (relation-page-count pgc "pg_class" "main"))
  (displayln (page-header-pagesize (get-page-header pgc "pg_class" "main" 0))))

(test)
