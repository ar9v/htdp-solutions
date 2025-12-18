#lang htdp/bsl

;;; Mathematics teachers may have introduced you to matrix calculations by now. In
;;; principle, matrix just means rectangle of numbers. Here is one possible data
;;; representation for matrices:

; A Matrix is one of:
;   -- (cons Row '())
;   -- (cons Row Matrix)
;
; contraint: all rows in a matrix are of the same length

; A Row is one of
;   -- '()
;   -- (cons Number Row)


;;; Study the data definition and translate the two-by-two matrix consisting of the numbers
;;; 11, 12, 21, and 22 into this data representation

;;; A:
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))


;;; The function in figure 70 implements the important mathematical operation of
;;; transposing the entries in a matrix. To transpose means to mirror the entries along
;;; the diagonal, that is, the line from the top-left to the bottom-right.

; Matrix -> Matrix
; transposes the given matrix along the diagonal
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
(check-expect (transpose mat1) tam1)
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

;;; Why does transpose ask `(empty? (first lln))`?

;;; A:
;;;
;;; A Matrix's `first` is a Row, which is either the empty list or `(cons Number Row)`.
;;; `transpose` essentially processes a matrix column-wise, meaning we pass around the
;;; matrix, but we're really interested in dealing with the structure of the row.

;;; This definition (figure 70) assumes two auxiliary functions:
;;;  -- first*, which consumes a matrix and produces the first column as a list of numbers
;;;  -- rest*, which consumes a matrix and removes the first column.
;;;
;;; The result is a matrix.
;;;
;;; Even though you lack definitions for these functions, you should be able to
;;; understand how transpose works. You should also understand that you cannot design this
;;; function with the design recipes you have seen so far. Explain why.

;;; A:
;;;
;;; Because so far, the design recipe has mapped strictly to our data definitions! In
;;; `transpose`, each `cond` branch isn't a branch of `Matrix`; it deals with the
;;; structure of a Row, and leverages the constraint that all of a Matrix's rows are
;;; the same size. It's not impossible to derive, but it does take a bit of cleverness
;;; to think outside of mapping our template 1-to-1 with our data definition.

;;; Design the two wish-list functions. Then complete the design of transpose with some
;;; test cases.

; first*: Matrix -> List-of-numbers
; Given Matrix `mat`, returns its first column, a list of numbers
(check-expect (first* mat1) (cons 11 (cons 21 '())))
(define (first* mat)
  (cond [(empty? (rest mat)) (cons (first (first mat)) '())]
        [else (cons (first (first mat))
                    (first* (rest mat)))]))

; rest*: Matrix -> Matrix
; Given a Matrix `mat`, return a smaller Matrix, `mat` without its first column
(check-expect (rest* mat1)
              (cons (cons 12 '())
                    (cons (cons 22 '())
                          '())))
(define (rest* mat)
  (cond [(empty? (rest mat)) (cons (rest (first mat)) '())]
        [else (cons (rest (first mat)) (rest* (rest mat)))]))
