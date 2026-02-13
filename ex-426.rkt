#lang htdp/isl+

;;; Complete the hand-evaluation from above. A close inspection of the evaluation suggests
;;; an additional trivial case for `quick-sort<`. Every time `quick-sort<` consumes a list
;;; of one item, it returns it as is. After all, the sorted version of a list of one item
;;; is the list itself.

;; (quick-sort< '(11 8 14 7))
;; ==
;; (append (quick-sort< (list 8 7))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (quick-sort< (list 7))
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (append (quick-sort< '())
;;                         (list 7)
;;                         (quick-sort< '()))
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (append '()
;;                         (list 7)
;;                         (quick-sort< '()))
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (append '()
;;                         (list 7)
;;                         '())
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (list 7)
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (list 7)
;;                 (list 8)
;;                 '())
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (append (quick-sort< '())
;;                 (list 14)
;;                 (quick-sort< '())))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (append '()
;;                 (list 14)
;;                 (quick-sort< '())))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (append '()
;;                 (list 14)
;;                 '()))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (list 14))
;; ==
;; '(7 8 11 14)
;;
;; 13 steps

;;; Modify `quick-sort<` to take advantage of this observation. Evaluate the example again.
;;; How many steps does the revised algorithm save?

; quick-sort<: [List-of Number] -> [List-of Number]
; Produces a sorted version of `alon`
; assume all the numbers are distinct
(define (quick-sort< alon)
  (cond [(or (empty? alon) (empty? (rest alon))) alon]
        [else
         (local [(define pivot (first alon))]
           (append (quick-sort< (smallers alon pivot))
                   (list pivot)
                   (quick-sort< (largers alon pivot))))]))

; largers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly larger than `n`
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

; smallers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly smaller than `n`
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

;; (quick-sort< '(11 8 14 7))
;; ==
;; (append (quick-sort< (list 8 7))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (quick-sort< (list 7))
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (list 7)
;;                 (list 8)
;;                 (quick-sort< '()))
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (append (list 7)
;;                 (list 8)
;;                 '())
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (quick-sort< (list 14)))
;; ==
;; (append (list 7 8)
;;         (list 11)
;;         (list 14))
;; ==
;; '(7 8 11 14)
;;
;; 7 steps

;;; The revised algorithm saved us 6 steps (skipping steps in the same way the book's
;;; example does). Generally, the revised algorithm saves us from having to call
;;; quick-sort< two extra times for each single-element list and then appending.
