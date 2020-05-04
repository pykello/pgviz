#lang racket

(require db
         db/util/postgresql
         "utils.rkt")

(provide (all-defined-out))

;;
;; data structures
;;

(struct page-header (lsn
                     checksum
                     flags
                     lower
                     upper
                     special
                     pagesize
                     version
                     prune_xid))

;; corresponds to ItemIdData in itemid.h
(struct item-id (lp_off
                 lp_flags
                 lp_len))

;; corresponds to HeapTupleHeaderData in htup_details.h
(struct htup-header (t_xmin
                     t_xmax
                     t_field3
                     t_ctid
                     t_infomask2
                     t_infomask
                     t_hoff
                     t_bits))

(struct heap-tuple (itemid
                    header
                    attrs))

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

(define (get-heap-tuples pgc relname fork idx)
  (define query "SELECT lp_off, lp_flags, lp_len, t_xmin::text, t_xmax::text,
                        t_field3, t_ctid::text, t_infomask2, t_infomask, t_hoff, t_bits, t_attrs
                 FROM heap_page_item_attrs(get_raw_page($1, $2, $3), $1::regclass)")
  (for/list ([row-v (query-rows pgc query relname fork idx)])
    (define row (vector->list row-v))
    (define itemid (apply item-id (take row 3)))
    (define header (apply htup-header (take (drop row 3) 8)))
    (define attrs (pg-array->list (car (drop row 11))))
    (heap-tuple itemid header attrs)))

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
  (displayln (page-header-pagesize (get-page-header pgc "pg_class" "main" 0)))
  (define pages (map heap-tuple-attrs (get-heap-tuples pgc "t" "main" 0)))
  (define first-page (map bytes->hex (first pages)))
  (displayln first-page))

;; (test)
