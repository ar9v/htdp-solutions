#lang htdp/bsl

;;; Here is one way to represent a phone number

(define OLD-AREA-CODE 713)
(define NEW-AREA-CODE 281)

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
;
; A Three is a Number between 100 and 999
; A Four is a Number between 1000 and 9999

;;; Design the function `replace`. It consumes and produces a list of Phones. It replaces
;;; all occurrence of area code 713 with 281.

; replace: List-of-Phone -> List-of-Phone
; Replaces all area codes 713 with 281 in `lop`
(check-expect (replace empty) empty)
(check-expect (replace (cons (make-phone 333 100 1000) empty))
              (cons (make-phone 333 100 1000) empty))
(check-expect (replace (cons (make-phone 333 100 1000)
                             (cons (make-phone 713 222 3333)
                                   (cons (make-phone 444 444 4444)
                                         (cons (make-phone 713 333 2222) empty)))))
               (cons (make-phone 333 100 1000)
                             (cons (make-phone 281 222 3333)
                                   (cons (make-phone 444 444 4444)
                                         (cons (make-phone 281 333 2222) empty)))))
(define (replace lop)
  (cond [(empty? lop) '()]
        [(cons? lop)
         (cons (if (= (phone-area (first lop)) OLD-AREA-CODE)
                   (phone-area-replace (first lop) NEW-AREA-CODE)
                   (first lop))
               (replace (rest lop)))]))

; phone-area-replace: Phone Number -> Phone
; Creates a new Phone, where `ph`'s area has been replaced with `new-area`
(check-expect (phone-area-replace (make-phone 123 111 2222) 321)
              (make-phone 321 111 2222))
(define (phone-area-replace ph new-area)
  (make-phone new-area (phone-switch ph) (phone-four ph)))
