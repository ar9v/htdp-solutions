#lang htdp/isl+

;;; Louise, Jane, Laura, Dana, and Mary decide to run a lottery that assings one gift
;;; recipient to each of them. Since Jane is a developer, they ask her to write a program
;;; that performs this task in an impartial manner. Of course, the program must not assign
;;; any of the sisters to herself.
;;;
;;; Here is the core of Jane's program:

(define ex-names '("bob" "carl" "john"))

; gift-pick: [List-of String] -> [List-of String]
; picks a random non-identity arrangement of names
(check-satisfied (gift-pick ex-names)
                 (λ (pick)
                   (and (not (string=? (list-ref pick 0) "bob"))
                        (not (string=? (list-ref pick 1) "carl"))
                        (not (string=? (list-ref pick 2) "john")))))
(define (gift-pick names)
  (random-pick (non-same names (arrangements names))))

; arrangements: [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(define (arrangements l)
  (local (; insert-everywhere/all: Any [List-of [List-of Any]] -> [List-of [List-of Any]]
          ; Produces a new list of lists, where `i` is inserted at the beginning,
          ; between each item and at the end of each item in `l`
          (define (insert-everywhere/all i lol)
            (local ((define (combine l res) (append (insert-everywhere/list i l) res)))
              (foldr combine '() lol)))

          ; insert-everywhere/list: Any [List-of Any] -> [List-of [List-of Any]]
          ; Takes `i` and inserts it into each possible place of `l`, producing a new list
          ; of words
          (define (insert-everywhere/list i l)
            (local ((define (combine x res)
                      (cons (cons i (cons x (rest (first res))))
                            (insert-at-front/lists x res))))
              (foldr combine (list (list i)) l)))

          ; insert-at-front/lists: Any [List-of [List-of Any]] -> [List-of [List-of Any]]
          ; Puts `i` in front (at the start) of each list in `lists`
          (define (insert-at-front/lists i ls)
            (local ((define (cons-i l) (cons i l))) (map cons-i ls))))

    (foldr insert-everywhere/all (list '()) l)))

;;; It consumes a list of names and randomly picks one of those permutations that do
;;; not agree with the original list at any place. Your task is to design two auxiliary
;;; functions: `random-pick` and `non-same`.

; random-pick: [NEList-of X] -> X
; returns a random item from the list
(define (random-pick l)
  (list-ref l (random (length l))))

; non-same: [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in `ll` that do not agree with `names` at any place
;
; Assumption: when non-empty, `ll` contains elements of the same length as `names`
(check-expect (non-same '() '()) '())
(check-expect (non-same '() '(("bob" "carl"))) '(("bob" "carl")))
(check-expect (non-same '("bob" "carl") '()) '())
(check-expect (non-same ex-names (arrangements ex-names))
              '(("carl" "john" "bob")
                ("john" "bob" "carl")))
(define (non-same names ll)
  (local [(define (non-same/arr names l) (andmap (λ (n i) (not (string=? n i))) names l))]
    (cond [(empty? ll) '()]
          [(empty? names) ll]
          [else
           (if (non-same/arr names (first ll))
               (cons (first ll) (non-same names (rest ll)))
               (non-same names (rest ll)))])))
