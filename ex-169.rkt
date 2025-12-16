#lang htdp/bsl

;;; Design the function `legal`. Like `translate` from exercise 168, the function consumes
;;; and produces a list of Posns. The result contains all those Posns whose x-coordinates
;;; are between 0 and 100 and whose y-coordinates are between 0 and 200.

(define empty-lop '())
(define lop
  (cons (make-posn -50 30)
        (cons (make-posn 0 0)
              (cons (make-posn 101 4)
                    (cons (make-posn 50 30)
                          (cons (make-posn 40 203)
                                (cons (make-posn 100 200)
                                      empty)))))))

(define all-legal-lop
  (cons (make-posn 0 0)
        (cons (make-posn 50 30)
              (cons (make-posn 100 200) empty))))

; legal: List-of-Posns -> List-of-Posns
; Filters out legal posns from `lop`.
(check-expect (legal empty-lop) empty-lop)
(check-expect (legal lop) all-legal-lop)
(define (legal lop)
  (cond [(empty? lop) '()]
        [(cons? lop)
         (if (legal? (first lop))
             (cons (first lop) (legal (rest lop)))
             (legal (rest lop)))]))

; legal?: Posn -> Boolean
; Given a pos, determines if
; -- x is between 0 and 100 ([0, 100]), and
; -- y is between 0 and 200 ([0, 200])
(check-expect (legal? (make-posn -50 30)) #false)
(check-expect (legal? (make-posn 0 0)) #true)
(check-expect (legal? (make-posn 101 4)) #false)
(check-expect (legal? (make-posn 50 30)) #true)
(check-expect (legal? (make-posn 100 200)) #true)
(check-expect (legal? (make-posn 100 -4)) #false)
(check-expect (legal? (make-posn 100 400)) #false)
(define (legal? pos)
  (and (<= 0 (posn-x pos) 100)
       (<= 0 (posn-y pos) 200)))
