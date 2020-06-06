#lang racket

(provide (all-defined-out))

(define (hex-format a-number (width 2) (prefix "0x"))
  (define digit-map "0123456789ABCDEF")
  (define digits
    (reverse
     (for/list ([i (in-range width)])
       (define d (remainder (quotient a-number (expt 16 i)) 16))
       (string-ref digit-map d))))
  (string-append prefix (apply string digits)))

(define (bytes->hex bs)
  (if (bytes? bs)
      (string-join (map hex-format (bytes->list bs)) " ")
      "(null)"))

(define (sublist lst idx len)
  (take (drop lst idx) len))

(define (substr s start end)
  (substring s start (min (string-length s) end)))

(define (compose f g)
  (λ (v)
    (f (g v))))

(define (delete-at lst idx)
  (cond
    [(>= idx (length lst)) lst]
    [else (append (take lst idx)
                  (drop lst (+ 1 idx)))]))

(define (index-of lst item)
  (define indexes
    (flatten
     (for/list ([s lst]
                [idx (in-range 0 (length lst))])
       (if (eq? item s)
           (list idx)
           (list)))))
  (if (null? indexes)
      #f
      (first indexes)))
