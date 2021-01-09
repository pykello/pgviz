#lang racket

(require libserialport
         "utils.rkt"
         "flashair.rkt")

(define (main)
  (flashair-connect-and-upload raspi-kernel-path "kernel.img")
  (displayln "Running tests ...")
  (define-values (in out)
    (open-serial-port "/dev/ttyUSB0" #:baudrate 115200))
  (define raspi-output (read-until in "Bye!" 15000))
  (displayln raspi-output)
  (test "Check SDHC Version" (λ ()
                               (string-contains? raspi-output
                                                 "vendor 0x99, sdversion 0x2, slot_status 0x0")))
  (test "Read Bytes" (λ ()
                       (equal? (take (find-byte-sequence raspi-output) 128)
                               (read-file-bytes sdcard-image-path 128))))
  (exit 0))

(main)
