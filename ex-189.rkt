#lang htdp/bsl+

;;; Here is the function `search`:

; search: Number List<Number> -> Boolean
(define (search n alon)
  (cond [(empty? alon) #false]
        [else (or (= (first alon) n) (search n (rest alon)))]))

;;; It determines whether some number occurs in a list of numbers. The function may have
;;; to traverse the entire list to find out that the number of interest isn't contained
;;; in the list.

;;; Develop the function `search-sorted`, which determines whether a number occurs in a
;;; sorted list of numbers. The function must take advantage of the fact that the list is
;;; sorted.

; search-sorted: Number List<Number> -> Boolean
; Searches for `n` in `alon`, which is sorted
(check-expect (search-sorted 1 '()) #false)
(check-expect (search-sorted 1 (list 1 2 3 4)) #true)
(check-expect (search-sorted 3 (list 1 2 3 4)) #true)
(check-expect (search-sorted 5 (list 1 2 3 4)) #false)
(check-expect (search-sorted 3 (list 1 2 4 5)) #false)
(define (search-sorted n alon)
  (cond [(empty? alon) #false]
        [(cons? alon)
         (and (not (< n (first alon)))
              (or (= n (first alon))
                  (search-sorted n (rest alon))))]))

;;; NOTE (spoilers): binary search is introduced in later chapters, for those who may
;;; be reading this knowing whilst knowing about it (it is possible to do it w/ bsl+, just
;;; a bit clunky w/o `let` and friends)
