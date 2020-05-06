#lang racket

(require db
         db/util/postgresql
         "../utils.rkt")

(provide (all-defined-out))

;;
;; data structures
;;

;; coresponds to PageHeaderData in bufpage.h
(struct page-header (offset
                     len
                     bytes
                     lsn
                     checksum
                     flags
                     lower
                     upper
                     special
                     pagesize
                     version
                     prune_xid
                     special-bytes))

;; corresponds to ItemIdData in itemid.h
(struct item-id (offset
                 len
                 bytes
                 lp_off
                 lp_flags
                 lp_len))

;; corresponds to HeapTupleHeaderData in htup_details.h
(struct htup-header (offset
                     len
                     bytes
                     t_xmin
                     t_xmax
                     t_field3
                     t_ctid
                     t_infomask2
                     t_infomask
                     t_hoff))

(struct heap-tuple (itemid
                    header
                    bits-offset
                    bits-bytes
                    t_bits
                    data-offset
                    data-len
                    data-bytes
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

(define page-header-size 24)
(define itemid-size 4)
(define htup-header-size 23)

(define (get-page-header pgc relname fork idx)
  (define bytes (take (bytes->list (get-raw-page pgc relname fork idx)) page-header-size))
  (define result
    (query-row pgc
               "WITH r AS (SELECT get_raw_page($1, $2, $3) bytes)
                SELECT lsn::text,
                       checksum,
                       flags,
                       lower,
                       upper,
                       special,
                       pagesize,
                       version,
                       prune_xid::text,
                       substring(r.bytes, special)
                FROM r, page_header(r.bytes)"
               relname fork idx))
  (apply page-header (append (list 0 page-header-size bytes) (vector->list result))))

(define (get-heap-tuples pgc relname fork idx)
  (define page-bytes (bytes->list (get-raw-page pgc relname fork idx)))
  (define query "SELECT lp_off, lp_flags, lp_len, t_xmin::text, t_xmax::text,
                        t_field3, t_ctid::text, t_infomask2, t_infomask, t_hoff, t_bits, t_attrs
                 FROM heap_page_item_attrs(get_raw_page($1, $2, $3), $1::regclass)
                 ORDER BY lp")
  (for/list ([row-v (query-rows pgc query relname fork idx)]
             [i (in-range 1024)])
    (define row (vector->list row-v))
    (define itemid-offset (+ page-header-size (* itemid-size i)))
    (define itemid-bytes (sublist page-bytes itemid-offset itemid-size))
    (define itemid (apply item-id (append (list itemid-offset itemid-size itemid-bytes) (take row 3))))
    (define htup-offset (item-id-lp_off itemid))
    (define htup-len (item-id-lp_len itemid))
    (define htup-bytes (sublist page-bytes htup-offset htup-len))
    (cond
      [(> htup-len 0)
       (let* [(htup-header-bytes (take htup-bytes htup-header-size))
              (header (apply htup-header
                             (append (list htup-offset htup-header-size htup-header-bytes)
                                     (sublist row 3 7))))
              (t_bits (list-ref row 10))
              (t_hoff (htup-header-t_hoff header))
              (bits-offset (+ htup-offset htup-header-size))
              (htup-bits-bytes (sublist htup-bytes htup-header-size (- t_hoff htup-header-size)))
              (htup-data-bytes (drop htup-bytes t_hoff))
              (attrs (pg-array->list (car (drop row 11))))
              (data-offset (+ htup-offset t_hoff))
              (data-len (- htup-len t_hoff))]
         (heap-tuple itemid header bits-offset htup-bits-bytes t_bits data-offset data-len htup-data-bytes attrs))]
      [else (heap-tuple itemid #f 0 `() "" 0 0 `() `())])))

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

(define (lp_flags->string f)
  (match f
    [0 "0: Unused"]
    [1 "1: Normal"]
    [2 "2: HOT Redirect"]
    [3 "3: Dead"]))

(define (t_infomask->string f)
  (define meanings `("HASNULL" "HASVARWIDTH" "HASEXTERNAL" "HASOID_OLD" "XMAX_KEYSHR_LOCK"
                               "COMBOCID" "XMAX_EXCL_LOCK" "XMAX_LOCK_ONLY" "XMAX_SHR_LOCK"
                               "XMIN_COMMITTED" "XMIN_INVALID" "XMIN_FROZEN" "XMAX_COMMITTED"
                               "XMAX_INVALID" "XMAX_IS_MULTI" "UPDATED" "MOVED_OFF" "MOVED_IN"))
  (string-append (hex-format f 4) ": \n"
                 (string-join (bits->strings f meanings) ", \n")))

(define (t_infomask2->string f)
  (string-append (hex-format f 4) ": \n"
                 (string-join
                  (flatten
                   (list
                    (string-append "Attributes: " (~a (bitwise-and f #x07ff)))
                    (if (bitwise-bit-set? f 13) "KEYS_UPDATED" `())
                    (if (bitwise-bit-set? f 14) "HOT_UPDATED" `())
                    (if (bitwise-bit-set? f 15) "HEAP_ONLY_TUPLE" `())))
                  ", \n")))

(define (pd_flags->string f)
  (define meanings `("HAS_FREE_LINES" "PAGE_FULL" "ALL_VISIBLE"))
  (string-append (hex-format f 1) ": \n"
                 (string-join (bits->strings f meanings) ", \n")))

(define (bits->strings f meanings)
  (flatten
   (for/list ([meaning meanings]
              [i (in-range 128)])
     (cond
       [(bitwise-bit-set? f i) (list meaning)]
       [else `()]))))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (displayln (bytes-length (get-raw-page pgc "pg_class" "main" 0)))
  (displayln (relation-page-count pgc "pg_class" "main"))
  (displayln (page-header-pagesize (get-page-header pgc "pg_class" "main" 0)))
  (define pages (map heap-tuple-attrs (get-heap-tuples pgc "x" "main" 0)))
  (define first-page (map bytes->hex (first pages)))
  (displayln first-page))

;;(test)
