#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `replace`, which substitutes the area code 713 with 281 in a list
;;; of phone records

(define OLD-AREA-CODE 713)
(define NEW-AREA-CODE 281)

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
;
; A Three is a Number between 100 and 999
; A Four is a Number between 1000 and 9999

; old-area-code?: Three -> Boolean
; Is this code OLD-AREA-CODE?
(define (old-area-code? code) (= code OLD-AREA-CODE))

; replace: [List-of Phone] -> [List-of Phone]
; Replaces all area codes 713 with 281 in `phones`
(check-expect (replace empty) empty)
(check-expect (replace (list (make-phone 333 100 1000)))
              (list (make-phone 333 100 1000)))
(check-expect (replace (list (make-phone 333 100 1000)
                             (make-phone 713 222 3333)
                             (make-phone 444 444 4444)
                             (make-phone 713 333 2222)))
              (list (make-phone 333 100 1000)
                    (make-phone 281 222 3333)
                    (make-phone 444 444 4444)
                    (make-phone 281 333 2222)))
(define (replace phones)
  (for/list [(p phones)]
    (match p
      [(phone (? old-area-code?) s f) (make-phone NEW-AREA-CODE s f)]
      [p p])))
