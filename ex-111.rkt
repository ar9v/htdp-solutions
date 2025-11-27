#lang htdp/bsl

;;; Take a look at these definitions

(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
;
; interpretation: represents a velocity vector

;;; Develop the function `checked-make-vec`, which is to be understood as a checked version
;;; of the primitive operation `make-vec`. It ensures that the arguments to `make-vec` are
;;; positive numbers. In other words, `checked-make-vec` enforces our informal data
;;; definition.

; checked-make-vec: Any Any -> VecOrError
; Calls `make-vec` with x and y if they are positive numbers. It signals an error
; otherwise.
(check-expect (checked-make-vec 1 1) (make-vec 1 1))
(check-error (checked-make-vec 0 0))
(check-error (checked-make-vec 0 1))
(check-error (checked-make-vec 1 0))
(check-error (checked-make-vec -1 1))
(define (checked-make-vec x y)
  (cond [(and (number? x) (number? y) (positive? x) (positive? y))
         (make-vec x y)]
        [else
         (error "make-vec: Expected x and y to be positive numbers")]))
