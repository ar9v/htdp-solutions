#lang htdp/isl+

;;; Design the `zip` function, which consumes a list of names, represented as strings, and
;;; a list of phone numbers, also strings. It combines those equally long lists into a list
;;; of phone records:

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

;;; Assume that the corresponding list items belong to the same person.

; zip: [List-of String] [List-of String] -> [List-of PhoneRecord]
; Combines `names` and `phones` into a list of PhoneRecord
;
; assumption: `names` and `phones` are the same size, and the phones are ordered to
; match their owners.
(check-expect (zip'("John" "Bob") '("123-123-1234" "234-234-2345"))
              (list (make-phone-record "John" "123-123-1234")
                    (make-phone-record "Bob" "234-234-2345")))
(define (zip names phones)
  (cond [(empty? names) '()]
        [else (cons (make-phone-record (first names) (first phones))
                    (zip (rest names) (rest phones)))]))
