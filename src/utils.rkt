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
  (string-join (map hex-format (bytes->list bs)) " "))

(define (sublist lst idx len)
  (take (drop lst idx) len))
