#lang htdp/isl+

;;; Confirm that DrRacket's stepper can deal with `lambda`. Use it to step through the
;;; third example and also determine how DrRacket evaluates the following expressions.

(define-struct ir [name price])
(define th 20)

((lambda (ir) (<= (ir-price ir) th))
 (make-ir "bear" 10))
;; ((lambda (ir) (<= (ir-price ir) th))
;;  (make-ir "bear" 10))
;; ==
;; (<= (ir-price (make-ir "bear" 10)) th)
;; ==
;; (<= 10 th)
;; ==
;; (<= 10 20)
;; ==
;; #true

(map (lambda (x) (* 10 x))
     '(1 2 3))
;; (... (* 10 1) ...)
;; ==
;; (... 10 ...)
;; ==
;; (... (* 10 2) ...)
;; ==
;; (... 20 ...)
;; ==
;; (... (* 10 3) ...)
;; ==
;; (... 30 ...)
;; ==
;; (... 30 ...)
;; ==
;; (list 10 20 30)

(foldl (lambda (name rst)
         (string-append name ", " rst))
       "etc."
       '("Matthew" "Robby"))
;; (... (string-append "Matthew" ", " "etc.") ...)
;; ==
;; (... "Matthew, etc." ...)
;; ==
;; (... (string-append "Robby" ", " "Matthew, etc.") ...)
;; ==
;; (... "Robby, Matthew, etc." ...)
;; ==
;; "Robby, Matthew, etc."

(filter (lambda (ir) (<= (ir-price ir) th))
        (list (make-ir "bear" 10)
              (make-ir "doll" 33)))
;; (... (<= (ir-price (make-ir "bear" 10)) th) ...)
;; ==
;; (... (<= 10 th) ...)
;; ==
;; (... (<= 10 20) ...)
;; ==
;; (... #true ...)
;; ==
;; (... (<= (ir-price (make-ir "doll" 33)) th) ...)
;; ==
;; (... (<= 33 th) ...)
;; ==
;; (... (<= 33 20) ...)
;; ==
;; (... #false ...)
;; ==
;; (list (make-ir "bear" 10))
