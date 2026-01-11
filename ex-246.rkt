#lang htdp/isl

;;; Check step 1 of the last calculation using the DrRacket stepper.

;; (extract < (cons 6 (cons 4 '())) 5)
;; ==
;; (extract < (cons 4 '()) 5)

(define (extract cmp l n)
  (cond [(empty? l) '()]
        [else (if (cmp (first l) n)
                  (cons (first l) (extract cmp (rest l) n))
                  (extract cmp (rest l) n))]))

;; (extract < (cons 6 (cons 4 '())) 5)
;; ==
;; (cond [(empty? (cons 6 (cons 4 '()))) '()]
;;       [else (if (< (first (cons 6 (cons 4 '()))) 5)
;;                 (cons (first (cons 6 (cons 4 '())))
;;                       (extract < (rest (cons 6 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 6 (cons 4 '()))) 5))])
;; ==
;; (cond [#false '()]
;;       [else (if (< (first (cons 6 (cons 4 '()))) 5)
;;                 (cons (first (cons 6 (cons 4 '())))
;;                       (extract < (rest (cons 6 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 6 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if (< (first (cons 6 (cons 4 '()))) 5)
;;                 (cons (first (cons 6 (cons 4 '())))
;;                       (extract < (rest (cons 6 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 6 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if (< 6 5)
;;                 (cons (first (cons 6 (cons 4 '())))
;;                       (extract < (rest (cons 6 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 6 (cons 4 '()))) 5))])
;; ==
;; (cond [else (if #false
;;                 (cons (first (cons 6 (cons 4 '())))
;;                       (extract < (rest (cons 6 (cons 4 '()))) 5))
;;                 (extract < (rest (cons 6 (cons 4 '()))) 5))])
;; ==
;; (cond [else (extract < (rest (cons 6 (cons 4 '()))) 5)])
;; ==
;; (extract < (rest (cons 6 (cons 4 '()))) 5)
;; ==
;; (extract < (cons 4 '()) 5)
