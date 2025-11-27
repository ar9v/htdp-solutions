#lang htdp/bsl

(require 2htdp/image) ; for empty-image

;;; Reforumlate the predicate now (`missile-or-not`) using an `or` expression
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)
(define (missile-or-not? v)
  (or (false? v) (posn? v)))
