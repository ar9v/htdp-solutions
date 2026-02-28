#lang htdp/isl+

;;; Test `find-path` on 'B, 'C, and the graph in figure 170. Also use `test-on-all-nodes`
;;; from exercise 472 on this graph.
;;;
;;; `(find-graph 'B 'C cyclic-graph)` succeeds, since the path is found before a cycle
;;; occurs. `test-on-all-nodes` does hang, however.

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

(define cyclic-graph
  '((A B E)
    (B E F)
    (C B D)
    (D)
    (E C F)
    (G)))

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

; test-on-all-nodes: Graph -> Boolean
; True if there'sa path between any pair of nodes in `G`
(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes connected-graph) #true)
(define (test-on-all-nodes G)
  (local [(define nodes (map first G))]
    (andmap (λ (o) (andmap (λ (d) (list? (find-path o d G))) nodes))
            nodes)))
