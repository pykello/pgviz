#lang racket

(require request
         net/url)

(provide (all-defined-out))

(define (flashair-connect-and-upload path name)
  (define ssid (wifi-current-ssid))
  (displayln (string-append "Current ssid is " ssid))
  (cond
    [(not (wifi-connect* "flashair"))
     (error "couldn't connect to flashair")]
    [(not (flashair-set-upload-dir "/"))
     (error "couldn't set upload dir")]
    [(not (flashair-upload path name))
     (error "couldn't upload file")]
    [(not (wifi-connect* ssid))
     (error "couldn't connect back to wifi")]
    [else #t]))

(define (flashair-set-upload-dir dir)
  (displayln "Setting upload dir ...")
  (define response (get http-requester
                        (string->url (string-append "http://flashair/upload.cgi?UPDIR=" dir))))
  (= (http-response-code response) 200))

(define (flashair-upload path name)
  (displayln "Uploading ...")
  (define CRLF "\r\n")
  (define boundary "-------------------------RacketFormBoundaryf1nfLeDGcfc30oHf")
  (define ->bytes string->bytes/utf-8)
  (define data
    (bytes-append
     (->bytes (string-append
               "--" boundary CRLF
               "Content-Disposition: form-data; name=\"file\"; filename=\"" name "\"" CRLF
               "Content-Type: application/octet-stream" CRLF
               CRLF))
     (file->bytes path)
     (->bytes (string-append CRLF "--" boundary "--" CRLF))))
  (define headers
    (list (string-append "Content-Type: multipart/form-data; boundary=" boundary)
          (string-append "Content-Length: " (number->string (bytes-length data)))))

  (define response (post http-requester
                         (string->url "http://flashair/upload.cgi")
                         data
                         #:headers headers))
  (= (http-response-code response) 200))

(define (wifi-connect* ssid)
  (displayln (string-append "Connecting to " ssid " ..."))
  (define password (wifi-saved-password ssid))
  (cond
    [(not (wifi-ssid-exists? ssid)) (wifi-rescan 5)])
  (wifi-connect ssid password))

(define (wifi-current-ssid)
  (string-trim
   (run-command "nmcli device wifi list | awk --field-separator='  +' '$1==\"*\" {printf(\"%s\", $2)}'")))

(define (wifi-connect ssid password)
  (define password-arg
    (if (non-empty-string? password)
        (string-append "password " password)
        ""))
  (define cmd (string-append "nmcli device wifi connect " ssid " " password-arg " 2>&1"))
  (not (string-contains? (run-command cmd) "Error")))

(define (wifi-saved-password ssid)
  (run-command
   (format "nmcli --show-secrets connection show \"~s\" | awk '$1==\"802-11-wireless-security.psk:\"{printf(\"%s\", $2)}'"
           ssid)))

(define (wifi-rescan [t 1])
  (run-command "nmcli device wifi rescan")
  (sleep t))

(define (wifi-list)
  (string-split
   (run-command "nmcli --mode=multiline device wifi list | awk --field-separator=' +' '$1==\"SSID:\"{print $2}'")
   "\n"))

(define (wifi-ssid-exists? ssid)
  (list? (member ssid (wifi-list))))

(define (run-command command)
  (with-output-to-string (λ()
                           (system command))))
