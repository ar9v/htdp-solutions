#lang htdp/isl+

;;; Test `find-path`. Use the function to find a path from 'A to 'G in `sample-graph`.
;;; Which one does it find? Why?
;;;
;;; A:
;;; It finds '(A B E F G)
;;;
;;; It finds this path because the algorithm processes `neighbors` in the order in which
;;; they are encoded in the graph. So, even if there are alternative routes, B is A's
;;; first neighbor, E is B's, F is E's and so on; since there actually is a path, that
;;; is the path that's returned.

; A Path is a [List-of Node]
; interpretation: The list of nodes specifies a sequence of immediate neighbors that
; leads from the first Node on the list to the last one.

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

(define connected-graph
  '((A B)
    (B C)
    (C A)))

; find-path: Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G. If there is no path, the function
; produces #false
(check-expect (find-path 'C 'D sample-graph) '(C D))
(check-member-of (find-path 'E 'D sample-graph) '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #false)
(define (find-path origination destination G)
  (cond [(symbol=? origination destination) (list destination)]
        [else
         (local [(define next (neighbors origination G))
                 (define candidate (find-path/list next destination G))]
           (cond [(boolean? candidate) #false]
                 [(cons? candidate) (cons origination candidate)]))]))

; find-path/list: [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-originations to destination; otherwise, it produces
; #false
(define (find-path/list lo D G)
  (cond [(empty? lo) #false]
        [else
         (local [(define candidate (find-path (first lo) D G))]
           (cond [(boolean? candidate) (find-path/list (rest lo) D G)]
                 [else candidate]))]))

; neighbors: Node Graph -> [Maybe [List-of Node]]
; Produces `n`'s neighbors in `g`, or #false if Node `n` is not in `g`
(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'F sample-graph) '(D G))
(check-expect (neighbors 'Z sample-graph) #false)
(define (neighbors n g)
  (local [(define maybe-assoc (assoc n g))]
    (if (false? maybe-assoc) maybe-assoc (rest maybe-assoc))))

;;; Design `test-on-all-nodes`, a function that consumes a graph `g` and determines whether
;;; there is a path between any pair of nodes.

; test-on-all-nodes: Graph -> Boolean
; True if there'sa path between any pair of nodes in `G`
(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes connected-graph) #true)
(define (test-on-all-nodes G)
  (local [(define nodes (map first G))]
    (andmap (λ (o) (andmap (λ (d) (list? (find-path o d G))) nodes))
            nodes)))
