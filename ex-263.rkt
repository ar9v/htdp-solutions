#lang htdp/isl

;;; Use DrRacket's stepper to study the steps of this calculation in detail.

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

;; I've included the steps here for completeness, but since the stepper will "nest" the
;; rename + hoist steps, it's way clearer to see this in the stepper.
;;
;; (inf '(2 1 3))
;; ==
;; (cond
;;   [(empty? (rest (list 2 1 3))) (first (list 2 1 3))]
;;   [else
;;    (local ((define smallest-in-rest (inf (rest 2 1 3))))
;;      (if (< (first (list 2 1 3)) smallest-in-rest)
;;          (first (list 2 1 3))
;;          smallest-in-rest))])
;; ==
;; (cond
;;   [(empty? (list 1 3)) (first (list 2 1 3))]
;;   [else
;;    (local ((define smallest-in-rest (inf (rest 2 1 3))))
;;      (if (< (first (list 2 1 3)) smallest-in-rest)
;;          (first (list 2 1 3))
;;          smallest-in-rest))])
;; ==
;; (cond
;;   [#false (first (list 2 1 3))]
;;   [else
;;    (local ((define smallest-in-rest (inf (rest 2 1 3))))
;;      (if (< (first (list 2 1 3)) smallest-in-rest)
;;          (first (list 2 1 3))
;;          smallest-in-rest))])
;; ==
;; (cond
;;   [else
;;    (local ((define smallest-in-rest (inf (rest 2 1 3))))
;;      (if (< (first (list 2 1 3)) smallest-in-rest)
;;          (first (list 2 1 3))
;;          smallest-in-rest))])
;; ==
;; (local ((define smallest-in-rest (inf (rest 2 1 3))))
;;   (if (< (first (list 2 1 3)) smallest-in-rest)
;;       (first (list 2 1 3))
;;       smallest-in-rest))
;; ==
;; (define smallest-in-rest_0 (inf (rest 2 1 3)))
;; ==
;; (define smallest-in-rest_0
;;   (cond
;;     [(empty? (rest (list 1 3))) (first (list 1 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 1 3)))))
;;        (if (< (first (list 1 3)) smallest-in-rest)
;;            (first (list 1 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_0
;;   (cond
;;     [(empty? (list 3)) (first (list 1 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 1 3)))))
;;        (if (< (first (list 1 3)) smallest-in-rest)
;;            (first (list 1 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_0
;;   (cond
;;     [#false (first (list 1 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 1 3)))))
;;        (if (< (first (list 1 3)) smallest-in-rest)
;;            (first (list 1 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_0
;;   (cond
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 1 3)))))
;;        (if (< (first (list 1 3)) smallest-in-rest)
;;            (first (list 1 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_0
;;   (local ((define smallest-in-rest (inf (rest (list 1 3)))))
;;     (if (< (first (list 1 3)) smallest-in-rest)
;;         (first (list 1 3))
;;         smallest-in-rest)))
;; ==
;; (define smallest-in-rest_1 (inf (rest (list 1 3))))
;; ==
;; (define smallest-in-rest_1 (inf (list 3)))
;; ==
;; (define smallest-in-rest_1
;;   (cond
;;     [(empty? (rest (list 3))) (first (list 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 3)))))
;;        (if (< (first (list 3)) smallest-in-rest)
;;            (first (list 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_1
;;   (cond
;;     [(empty? '()) (first (list 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 3)))))
;;        (if (< (first (list 3)) smallest-in-rest)
;;            (first (list 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_1
;;   (cond
;;     [#true (first (list 3))]
;;     [else
;;      (local ((define smallest-in-rest (inf (rest (list 3)))))
;;        (if (< (first (list 3)) smallest-in-rest)
;;            (first (list 3))
;;            smallest-in-rest))]))
;; ==
;; (define smallest-in-rest_1
;;   (first (list 3)))
;; ==
;; (define smallest-in-rest_1 3)
;; ==
;; (define smallest-in-rest_0
;;   (if (< (first (list 1 3)) smallest-in-rest_1)
;;       (first (list 1 3))
;;       smallest-in-rest_1))
;; ==
;; (define smallest-in-rest_0
;;   (if (< 1 smallest-in-rest_1)
;;       (first (list 1 3))
;;       smallest-in-rest_1))
;; ==
;; (define smallest-in-rest_0
;;   (if (< 1 3)
;;       (first (list 1 3))
;;       smallest-in-rest_1))
;; ==
;; (define smallest-in-rest_0
;;   (if #true
;;       (first (list 1 3))
;;       smallest-in-rest_1))
;; ==
;; (define smallest-in-rest_0
;;   (first (list 1 3)))
;; ==
;; (define smallest-in-rest_0 1)
;; ==
;; (if (< (first (list 2 1 3)) smallest-in-rest_0)
;;     (first (list 2 1 3))
;;     smallest-in-rest_0)
;; ==
;; (if (< 2 smallest-in-rest_0)
;;     (first (list 2 1 3))
;;     smallest-in-rest_0)
;; ==
;; (if (< 2 1)
;;     (first (list 2 1 3))
;;     smallest-in-rest_0)
;; ==
;; (if #false
;;     (first (list 2 1 3))
;;     smallest-in-rest_0)
;; ==
;; smallest-in-rest_0
;; ==
;; 1
