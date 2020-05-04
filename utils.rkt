#lang racket

(provide (all-defined-out))

(define (hex-format a-number (width 2))
  (define digit-map "0123456789ABCDEF")
  (define digits
    (reverse
     (for/list ([i (in-range width)])
       (define d (remainder (quotient a-number (expt 16 i)) 16))
       (string-ref digit-map d))))
  (string-append "0x" (apply string digits)))

(define (bytes->hex bs)
  (string-join (map hex-format (bytes->list bs)) " "))
