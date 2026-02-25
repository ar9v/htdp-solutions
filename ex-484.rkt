#lang htdp/isl+

;;; While a list sorted in descending order is clearly the worst possible input for
;;; `inf`, the analysis of `inf`'s abstract running time explains why the rewrite of `inf`
;;; with `local` reduces the running time. For convenience, we replicate this version
;;; here:

(define (infL l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define s (infL (rest l))))
            (if (< (first l) s) (first l) s))]))

;;; Hand-evaluate `(infL (list 3 2 1 0))`. Then argue that `infL` uses on the "order of n
;;; steps" in the best and worst case. You may now wish to revisit exercise 261, which
;;; asks you to explore a similar problem.
;;;
;;; A:
;;; We call `inf` recursively one time per element in the list save for the base case.
;;; Once we hit the base case, the stack unwinds, comparing the result with `(first l)`
;;; for a given frame, operations which we assume to be constant. Since we perform a
;;; "unitary" task `n` times (where n is the number of elements), then we can say that
;;; `infL` takes on the order of n steps.

;; (infL (list 3 2 1 0))
;; ==
;; (local ((define s (infL (rest '(3 2 1 0)))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s (infL (rest '(2 1 0)))))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s
;;                     (local ((define s (infL (rest '(1 0)))))
;;                       (if (< (first '(1 0)) s) (first '(1 0)) s))))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s
;;                     (local ((define s
;;                               (local ((define s (infL (rest '(0)))))
;;                                 (if (< (first '(0)) s) (first '(0)) s))))
;;                       (if (< (first '(1 0)) s) (first '(1 0)) s))))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s
;;                     (local ((define s
;;                               (local ((define s (first '(0))))
;;                                 (if (< (first '(0)) s) (first '(0)) s))))
;;                       (if (< (first '(1 0)) s) (first '(1 0)) s))))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s
;;                     (local ((define s 0))
;;                       (if (< (first '(1 0)) s) (first '(1 0)) s))))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s
;;           (local ((define s 0))
;;             (if (< (first '(2 1 0)) s) (first '(2 1 0)) s))))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; (local ((define s 0))
;;   (if (< (first '(3 2 1 0)) s) (first '(3 2 1 0)) s))
;; ==
;; 0
