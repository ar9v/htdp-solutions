#lang htdp/isl+

(require 2htdp/batch-io)

;;; Add `sort<` and `quick-sort<` to the definitions area. Run tests on the functions
;;; to ensure that they work on basic examples. Also develop `create-tests`, a function
;;; that creates large test cases randomly. Then explore how fast each works on various
;;; lists.
;;;
;;; Does the experiment confirm the claim that the plain `sort<` function often wins
;;; over `quick-sort<` for short lists and vice versa?
;;;
;;; Determine the cross-over point. Use it to build a `clever-sort` function that behaves
;;; like `quick-sort<` for large lists and like `sort<` for lists below this cross-over
;;; point. Compare with exercise 427.

(define MAX 1000)

; quick-sort<: [List-of Number] -> [List-of Number]
; Produces a sorted version of `alon`
; assume all the numbers are distinct
(check-expect (quick-sort< '(1 4 9 6 3 5 2)) '(1 2 3 4 5 6 9))
(check-expect (quick-sort< '(5 5 -2 1 0 5 -3)) '(-3 -2 0 1 5 5 5))
(define (quick-sort< alon)
  (local [(define (smallers alon pivot) (filter (λ (x) (<= x pivot)) alon))
          (define (largers alon pivot) (filter (λ (x) (> x pivot)) alon))]
    (cond [(empty? alon) '()]
          [else
           (local [(define pivot (first alon))
                   (define rst (rest alon))]
             (append (quick-sort< (smallers rst pivot))
                     (list pivot)
                     (quick-sort< (largers rst pivot))))])))

; sort<: [List-of Number] -> [List-of Number]
; Produces a sorted version of `alon`, but uses the insertion sort algorithm
(check-expect (quick-sort< '(1 4 9 6 3 5 2)) '(1 2 3 4 5 6 9))
(check-expect (quick-sort< '(5 5 -2 1 0 5 -3)) '(-3 -2 0 1 5 5 5))
(define (sort< alon)
  (local [(define (insert x l)
            (cond [(empty? l) (list x)]
                  [else
                   (if (< x (first l))
                       (cons x l)
                       (cons (first l) (insert x (rest l))))]))]
    (foldr insert '() alon)))

(define (create-tests n) (build-list n (λ (_i) (random MAX))))

(define (run-tests sizes fn)
  (local [(define inputs (map (λ (size) (list size (create-tests size))) sizes))]
    (for-each
     (λ (size-list)
       ; I'm abusing `local` to simulate statements; don't do this at home kids
       (local [(define _size
                 (write-file
                  'stdout
                  (string-append (number->string (first size-list)) "\n")))
               (define _res (time (fn (second size-list))))]
         (write-file 'stdout "\n")))
     inputs)))

;; ex-442.rkt> (run-tests '(10 50 75 100 150 200 500))
;;
;; sort<: cpu time: 0 real time: 0 gc time: 0
;; qsort<: cpu time: 0 real time: 0 gc time: 0
;; 10
;; sort<: cpu time: 1 real time: 1 gc time: 0
;; qsort<: cpu time: 1 real time: 1 gc time: 0
;; 50
;; sort<: cpu time: 3 real time: 3 gc time: 0
;; qsort<: cpu time: 1 real time: 1 gc time: 0
;; 75
;; sort<: cpu time: 4 real time: 4 gc time: 0
;; qsort<: cpu time: 2 real time: 2 gc time: 0
;; 100
;; sort<: cpu time: 9 real time: 9 gc time: 0
;; qsort<: cpu time: 4 real time: 4 gc time: 0
;; 150
;; sort<: cpu time: 18 real time: 18 gc time: 0
;; qsort<: cpu time: 5 real time: 5 gc time: 0
;; 200
;; sort<: cpu time: 92 real time: 92 gc time: 0
;; qsort<: cpu time: 5 real time: 5 gc time: 0
;; 500
;;
;; We can see that at around ~75 items, quicksort starts beating `sort<` by a decent factor.
;; To take advantage of `sort<`, we'd then use a threshold of ~50 items.

; clever-sort: [List-of Number] -> [List-of Number]
(check-expect (clever-sort '(1 4 9 6 3 5 2)) '(1 2 3 4 5 6 9))
(check-expect (clever-sort '(5 5 -2 1 0 5 -3)) '(-3 -2 0 1 5 5 5))
(define (clever-sort alon)
  (local [(define threshold 50)
          (define (smallers l p) (filter (λ (x) (<= x p)) l))
          (define (largers l p) (filter (λ (x) (> x p)) l))]
    (cond [(<= (length alon) threshold) (sort< alon)]
          [else (local [(define pivot (first alon))
                        (define rst (rest alon))]
                  (append (clever-sort (smallers rst pivot))
                          (list pivot)
                          (clever-sort (largers rst pivot))))])))
