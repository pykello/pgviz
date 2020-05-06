#lang racket/gui

(provide heap-page-view)

(require "../gui-components/memory-view.rkt"
         "../postgres/pageinspect.rkt"
         "../utils.rkt"
         db)

(define header-brush "Medium Goldenrod")
(define itemid-brush1 "LightSkyBlue")
(define itemid-brush2 "Medium Turquoise")
(define htup-brush "NavajoWhite")
(define htup-header-brush (make-object brush% "NavajoWhite" 'bdiagonal-hatch))
(define htup-bits-brush (make-object brush% "VioletRed" 'bdiagonal-hatch))

(define (heap-page-view pgc rel fork idx set-attrs)
  (define legend `((,header-brush "Page Header")
                            (,itemid-brush1 "ItemId")
                            (,itemid-brush2 "ItemId")
                            (,htup-brush "Heap Tuple Data")
                            (,htup-header-brush "Heap Tuple Header")
                            (,htup-bits-brush "Heap Null Bitmap")))

  (define page-header (get-page-header pgc rel fork idx))
  (define page-size (page-header-pagesize page-header))
  (define header-cells (page-header->memory-cells page-header set-attrs))
  (define tuple-cells (htups->memory-cells (get-heap-tuples pgc rel fork idx) set-attrs))
  (define cells (append header-cells tuple-cells))
  (new memory-view%
       [cells cells]
       [legend legend]
       [memory-size page-size]))

(define (page-header->memory-cells header set-attrs)
  (define callback
    (λ ()
      (set-attrs `(("Type" "Page Header")
                   ("LSN" ,(page-header-lsn header))
                   ("checksum" ,(page-header-checksum header))
                   ("flags" ,(pd_flags->string (page-header-flags header)))
                   ("lower" ,(page-header-lower header))
                   ("upper" ,(page-header-upper header))
                   ("special" ,(page-header-special header))
                   ("pagesize" ,(page-header-pagesize header))
                   ("version" ,(page-header-version header))
                   ("prune_xid" ,(page-header-version header))))))
  (define header-cells
    (for/list ([value (page-header-bytes header)]
             [addr (in-range (page-header-offset header) (page-header-len header))])
    (memory-cell addr value header-brush callback)))

  (define special-callback
    (λ ()
      (set-attrs `(("Type" "Special")))))
  (define special-cells
    (for/list ([value (page-header-special-bytes header)]
               [addr (in-range (page-header-special header) (page-header-pagesize header))])
      (memory-cell addr value header-brush special-callback)))

  (append header-cells special-cells))

(define (htups->memory-cells htups set-attrs)
  (flatten
   (for/list ([tup htups]
              [idx (in-range 1024)])
     (define itemid-color (if (odd? idx) itemid-brush1 itemid-brush2))
     (define itemid (heap-tuple-itemid tup))
     (define itemid-from (item-id-offset itemid))
     (define itemid-callback
       (λ ()
         (set-attrs `(("Type" "ItemId")
                      ("lp_off" ,(hex-format (item-id-lp_off itemid) 4))
                      ("lp_len" ,(item-id-lp_len itemid))
                      ("lp_flags" ,(lp_flags->string (item-id-lp_flags itemid)))))))
     (define itemid-cells (bytes->cells (item-id-bytes itemid)
                                        itemid-from
                                        itemid-color
                                        itemid-callback))

     (define header (heap-tuple-header tup))
     (define header-callback
       (λ ()
         (set-attrs `(("Type" "Heap Tuple Header")
                      ("t_xmin" ,(htup-header-t_xmin header))
                      ("t_xmax" ,(htup-header-t_xmax header))
                      ("t_field3" ,(htup-header-t_field3 header))
                      ("t_ctid" ,(htup-header-t_ctid header))
                      ("t_infomask2" ,(t_infomask2->string (htup-header-t_infomask2 header)))
                      ("t_infomask" ,(t_infomask->string (htup-header-t_infomask header)))
                      ("t_hoff" ,(htup-header-t_hoff header))
                      ("t_bits" "")))))
                      
     (define header-cells
       (cond
         [(eq? header #f) `()]
         [else (bytes->cells (htup-header-bytes header)
                             (htup-header-offset header)
                             htup-header-brush
                             header-callback)]))

     (define t_bits (heap-tuple-t_bits tup))
     (define bits
       (for/list ([bit (string->list (~a t_bits))]
                  [i (in-range 1024)])
         (list (string-append "t_bits[" (~a i) "]")
               (substr (~a bit) 0 100))))

     (define bits-callback
       (λ ()
         (set-attrs (cons `("Type" "Heap Null Bitmap")
                          (if (sql-null? t_bits)
                              `(("t_bits" "(null)"))
                              bits)))))

     (define bits-cells
       (bytes->cells (heap-tuple-bits-bytes tup)
                     (heap-tuple-bits-offset tup)
                     htup-bits-brush
                     bits-callback))

     (define attrs
       (for/list ([attr (heap-tuple-attrs tup)]
                  [i (in-range 1024)])
         (list (string-append "attrs[" (~a i) "]")
               (substr (bytes->hex attr) 0 100))))

     (define tuple-callback
       (λ ()
         (set-attrs (cons `("Type" "Heap Tuple Data") attrs))))

     (define data-cells (bytes->cells (heap-tuple-data-bytes tup)
                                      (heap-tuple-data-offset tup)
                                      htup-brush
                                      tuple-callback))
     (append itemid-cells header-cells bits-cells data-cells))))

(define (bytes->cells bytes from color callback)
  (define len (length bytes))
  (define to (+ from len))
  (for/list ([value bytes]
             [addr (in-range from to)])
    (memory-cell addr value color callback)))
