#lang racket

(require "utils.rkt")

(define (main)
  (define qemu-output
    (run-qemu #:kernel kernel-path
              #:sdcard sdcard-image-path))
  (test "Check SDHC Version" (λ ()
                               (string-contains? qemu-output
                                                 "vendor 0x24, sdversion 0x1, slot_status 0x0")))
  (test "Read Bytes" (λ ()
                       (equal? (find-byte-sequence qemu-output)                
                               (read-file-bytes sdcard-image-path 512))))
  (exit 0))

(define (run-qemu #:kernel kernel #:sdcard sdcard)
  (match-define (list stdout stdin pid stderr ctl)
    (process* "/usr/bin/qemu-system-arm" "-M" "raspi2" "-m" "512M" "-nographic"
              "-sd" sdcard
              "-kernel" kernel))
  (cond
    [(eq? (ctl 'status) 'done-error) (error (read-string 4096 stderr))]
    [else (define output2 (read-until stdout "Bye!" 4000))
          (exit-qemu stdin)
          (ctl 'wait)
          output2]))

;; writes the qemu exit sequence to stdin and closes it
(define (exit-qemu stdin)
  (write-byte 1 stdin)
  (write-byte 120 stdin)
  (close-output-port stdin))

(main)
