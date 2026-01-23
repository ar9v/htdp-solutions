#lang htdp/isl+

;;; A program that consists of three connected functions ought to express this
;;; relationship with a local expression. Copy and reorganize the program from figure 117
;;; into a single function using local.
;;;
;;; Validate the revised code with the test suite for count. The second argument to the
;;; local functions, `sy`, never changes. It is always the same as the original symbol.
;;; Hence you can eliminate it from the local function definitions to tell the reader that
;;; `sy` is a constant across the traversal process.

; atom?: S-exp -> Boolean
(define (atom? s-exp)
  (or (number? s-exp) (string? s-exp) (symbol? s-exp)))

; count: S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local [(define (count-sl sl)
            (cond [(empty? sl) 0]
                  [else (+ (count (first sl) sy)
                           (count-sl (rest sl)))]))

          (define (count-atom at)
            (cond [(number? at) 0]
                  [(string? at) 0]
                  [(symbol? at) (if (symbol=? at sy) 1 0)]))]
    (cond [(atom? sexp) (count-atom sexp)]
          [else (count-sl sexp)])))
