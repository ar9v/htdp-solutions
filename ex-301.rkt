#lang htdp/isl+

;;; Draw a box around the scope of each binding occurrence of `sort` and `alon` in figure
;;; 105. Then draw arrows from each occurrence of `sort` to the appropriate binding
;;; occurrence.

;; (define (insertion-sort alon)
;; +--------------------------------------------------------+
;; | (local ((define (sort alon)                            |
;; |      +------------|---------------+                    |
;; |      |   +------------------------+------------------+ |
;; |      |   |(cond                   |                  | |
;; |      |   |  [(empty? alon) '()]   |                  | |
;; |      |   |  [else                 v                  | |
;; |      |   |   (add (first alon) (sort (rest alon)))]))| |
;; |      |   +-------------------------------------------+ |
;; |      |  (define (add an alon)                          |
;; |      |  +---------------------------------------------+|
;; |      |  | (cond                                       ||
;; |      |  |   [(empty? alon) (list an)]                 ||
;; |      |  |   [else                                     ||
;; |      |  |    (cond                                    ||
;; |      |  |      [(> an (first alon)) (cons an alon)]   ||
;; |      |  |      [else (cons (first alon)               ||
;; |      |  |                  (add an (rest alon)))])])))||
;; |      |  +---------------------------------------------+|
;; |      v                                                 |
;; |   (sort alon)))                                        |
;; +--------------------------------------------------------+

;;; Now repeat the exercise for the variant in figure 106.

;; (define (sort alon)
;; +-------------------------------------------------------------+
;; | (local ((define (sort alon)                                 |
;; |     +-------------|--------------+                          |
;; |     |  +-------------------------+----------------------+   |
;; |     |  |  (cond                  |                      |   |
;; |     |  |    [(empty? alon) '()]  |                      |   |
;; |     |  |    [else                v                      |   |
;; |     |  |     (add (first alon) (sort (rest alon)))]))   |   |
;; |     |  +------------------------------------------------+   |
;; |     |   (define (add an alon)                               |
;; |     |    +----------------------------------------------+   |
;; |     |    |(cond                                         |   |
;; |     |    |  [(empty? alon) (list an)]                   |   |
;; |     |    |  [else                                       |   |
;; |     |    |   (cond                                      |   |
;; |     |    |     [(> an (first alon)) (cons an alon)]     |   |
;; |     |    |     [else (cons (first alon)                 |   |
;; |     |    |                 (add an (rest alon)))])])))  |   |
;; |     v    +----------------------------------------------+   |
;; |   (sort alon)))                                             |
;; +-------------------------------------------------------------+

;;; Do the two functions differ other than in name?
;;;
;;; A: They don't (!)
