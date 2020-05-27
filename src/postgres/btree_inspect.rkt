#lang racket

(require db
         db/util/postgresql
         "../utils.rkt")

(provide (all-defined-out))

;; corresponds to BTMetaPageData in nbtree.h
(struct btree-meta (magic
                    version
                    root
                    level
                    fastroot
                    fastlevel
                    oldest_xact
                    last_cleanup_num_tuples))

(define btree%
  (class object%
    (init-field relname
                pgc)

    ;;
    ;; public fields
    ;;
    (define/public (get-meta-page)
      (define result
        (vector->list
         (query-row pgc "SELECT * FROM bt_metap($1)" relname)))
      (apply btree-meta result))

    (define/public (get-root)
      (define metap (send this get-meta-page))
      (define root-blkno (btree-meta-root metap))
      (new btree-node%
           [blkno root-blkno]
           [relname relname]
           [pgc pgc]))

    (super-new)))


(define btree-node%
  (class object%

    (init-field relname
                blkno
                pgc)

    ;;
    ;; public methods 
    ;;

    (define (query-stats)
      (vector->list
       (query-row pgc "SELECT * FROM bt_page_stats($1, $2)" relname blkno)))

    (define (text->tid t)
      (define tokens
        (string-split (regexp-replace* #rx"\\(|\\)|," t " ")))
      (map string->number tokens))

    (define (data->text type data)
      (define bytes (map (λ(x) (string->number x 16)) (string-split data)))
      (define bytes-rest
        (if (null? bytes)
            `()
            (cdr bytes)))
      (match type
        [(or `(20) `(21) `(23)) (bytes->num bytes)]
        [(or `(25)) (bytes->text bytes-rest)]
        [_ data]))

    (define (bytes->num bs)
      (cond
        [(empty? bs) 0]
        [else (+ (car bs) (* 256 (bytes->num (cdr bs))))]))

    (define (bytes->text bs)
      (cond
        [(empty? bs) ""]
        [(eq? (first bs) 0) ""]
        [(string-append (string (integer->char (first bs)))
                        (bytes->text (cdr bs)))]))

    (define/public (get-type)
      (match (second (query-stats))
        [#\r 'root]
        [#\l 'leaf]
        [#\i 'internal]))

    (define/public (has-high-key?)
      (define btpo_next (ninth (query-stats)))
      (> btpo_next 0))

    (define/public (get-attr-types)
      (define query-result
        (query-rows pgc "SELECT atttypid::int FROM pg_attribute, pg_class
                         WHERE pg_class.oid=pg_attribute.attrelid AND relname=$1
                         ORDER BY attnum"
                    relname))
      (map (λ(r) (vector-ref r 0)) query-result))

    (define/public (get-items)
      (define query-result
        (query-rows pgc "SELECT ctid::text, data, itemlen
                         FROM bt_page_items($1, $2)" relname blkno))
      (define my-type (send this get-type))
      (define attr-types (send this get-attr-types))
      (define falses (build-list (length query-result)
                                 (λ(x) #f)))
      (define first?-list
        (if (send this has-high-key?)
            (cons #f (cons #t falses))
            (cons #t falses)))
      (for/list ([r query-result]
                 [first? first?-list])
        (define ctid (text->tid (vector-ref r 0)))
        (define data (vector-ref r 1))
        (define data-text
          (if (and first? (not (eq? my-type 'leaf)))
              "-∞"
              (~a (data->text attr-types data))))
        (match my-type
          ['leaf (cons data-text ctid)]
          [_ (cons data-text (new btree-node%
                             [relname relname]
                             [pgc pgc]
                             [blkno (car ctid)]))])))

    (super-new)))

(define (test)
  (define pgc
    (postgresql-connect #:user "hadi"
                        #:database "postgres"))
  (define btree (new btree%
                    [relname "t_idx"]
                    [pgc pgc]))
  (define root (send btree get-root))
  (define root-items (send root get-items))
  (define attr-types (send root get-attr-types))
  (displayln root-items)
  (displayln attr-types))

;;(test)
