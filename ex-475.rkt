#lang htdp/isl+

(require 2htdp/abstraction)

;;; Redesign `find-path/list` so that it uses an existing list abstraction from figures
;;; 95 and 96 instead of explicit structural recursion.
;;;
;;; HINT: Read the documentation for Racket's `ormap`. How does it differ from ISL+'s
;;; `ormap` function? Would the former be helpful here?
;;;
;;; A:
;;; The difference is that Racket's `ormap` returns the first non-false value, whereas
;;; ISL's version produces a Boolean. It would be helpful, since we're looking for
;;; either a path or #false. We can, in fact, get this behavior by using
;;; 2htdp/abstraction's `for/or` (!)
;;;
;;; It _is_ possible to use a fold.
;;;
;;; (foldr (λ (o _res) (local [(define candidate (find-path o D G))]
;;;                      (if (list? candidate) candidate #false)))
;;;        #false
;;;        lo)
;;;
;;; but this (1) is more verbose and (2) forces us to go through all origins.

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; find-path: Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G. If there is no path, the function
; produces #false
(check-expect (find-path 'C 'D sample-graph) '(C D))
(check-member-of (find-path 'E 'D sample-graph) '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #false)
(define (find-path origination destination G)
  (local [(define (find-path/list lo D G) (for/or [(o lo)] (find-path o D G)))
          (define (neighbors n g)
            (local [(define maybe-assoc (assoc n g))]
              (if (false? maybe-assoc) maybe-assoc (rest maybe-assoc))))]
    (cond [(symbol=? origination destination) (list destination)]
          [else (local [(define next (neighbors origination G))
                        (define candidate (find-path/list next destination G))]
                  (cond [(boolean? candidate) #false]
                        [(cons? candidate) (cons origination candidate)]))])))
