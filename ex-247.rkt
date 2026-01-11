#lang htdp/isl

;;; Evaluate `(extract < (cons 8 (cons 4 '())) 5)` with DrRacket's stepper


;; (extract < (cons 8 (cons 4 '())) 5)
;; ==
;; (cond [(empty? (cons 8 (cons 4 '()))) '()]
;;       [else (if (< (first (cons 8 (cons 4 '()))) 5)
;;                 (cons (first (cons 8 (cons 4 '())))
;;                       (extract < (rest (cons 8 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 8 (cons 4 '()))) 5))])
;; ==
;; (cond [#false '()]
;;       [else (if (< (first (cons 8 (cons 4 '()))) 5)
;;                 (cons (first (cons 8 (cons 4 '())))
;;                       (extract < (rest (cons 8 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 8 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if (< (first (cons 8 (cons 4 '()))) 5)
;;                 (cons (first (cons 8 (cons 4 '())))
;;                       (extract < (rest (cons 8 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 8 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if (< 8 5)
;;                 (cons (first (cons 8 (cons 4 '())))
;;                       (extract < (rest (cons 8 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 8 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if #false
;;                 (cons (first (cons 8 (cons 4 '())))
;;                       (extract < (rest (cons 8 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 8 (cons 4 '()))) 5))])
;; ==
;; (cond [else (extract < (rest (cons 8 (cons 4 '()))) 5)])
;; ==
;; (extract < (rest (cons 8 (cons 4 '()))) 5)
;; ==
;; (extract < (cons 4 '()) 5)
;; ==
;; (list 4) ; Law of equivalence, since the book has already established this result
