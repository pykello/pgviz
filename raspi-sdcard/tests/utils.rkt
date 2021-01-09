#lang racket

(provide (all-defined-out))

;; finds the line starting with "BYTES: " in the given string
;; and parses it as a byte list.
(define (find-byte-sequence s)
  (define lines (string-split s "\n"))
  (match (findf (λ (line) (string-prefix? line "BYTES: ")) lines)
    [#f '()]
    [s (define hex-list (cdr (string-split s)))
       (map hex->byte hex-list)]))

;; Converts numbers in 0x1f format to decimal
(define (hex->byte h)
  (define (hex-digits->byte lst)
    (cond
      [(null? lst) 0]
      [else (+ (* 16 (hex-digits->byte (cdr lst)))
               (hex-char->int (car lst)))]))

  (define (hex-char->int c)
    (cond ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
          ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
          ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))))

  (hex-digits->byte (reverse (string->list (string-trim h "0x")))))

;; Reads from port until reaching exit-seq or timing out
(define (read-until port exit-seq timeout)
  (define bs (make-bytes 65536))
  (define expire (+ (current-inexact-milliseconds) timeout))
  (define (aux idx)
    (define end-pos (min (+ idx 4096) (bytes-length bs)))
    (cond
      [(> (current-inexact-milliseconds) expire)
       (error "Reading from port timed out")]
      [else (define len (read-bytes-avail! bs port idx end-pos))
            (cond
              [(string-contains? (bytes->string/latin-1 bs) exit-seq) (void)]
              [else (sleep 0.01)
                    (aux (+ idx len))])]))
  (aux 0)
  (bytes->string/latin-1 bs))

;; Reads len bytes from the file
(define (read-file-bytes path len)
  (define f (open-input-file path))
  (bytes->list (read-bytes len f)))

;; Runs f, and prints its success result
(define (test name f)
  (display (string-append name " ... "))
  (if (f)
      (displayln "Passed")
      (displayln "Failed")))

(define tests-directory
  (path-only (path->complete-path (find-system-path 'run-file))))

(define sdcard-image-path
  (path->string (build-path tests-directory "sdcard.img")))

(define kernel-path
  (path->string
   (simplify-path
    (build-path tests-directory 'up "raspi2-qemu.img"))))

(define raspi-kernel-path
  (path->string
   (simplify-path
    (build-path tests-directory 'up "raspi-kernel.img"))))