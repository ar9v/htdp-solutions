#lang htdp/isl+

;;; Translate one of the above definitions into proper list form using `list` and
;;; proper symbols.

(check-expect
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ()))
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D (list))
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G (list))))

(define graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))
(check-expect
 '((A B E)
   (B E F)
   (C D)
   (D)
   (E C F)
   (F D G)
   (G))
 graph)

;;; The data representation for nodes is straightforward:

; A Node is a Symbol

;;; Formulate a data definition to describe the class of all Graph representations,
;;; allowing an arbitrary number of nodes and edges. Only one of the above representations
;;; has to belong to Graph.

; A Graph is a [List-of [List-of Node]]

;;; Design the function `neighbors`. It consumes a Node `n` and a Graph `g` and produces
;;; the of immediate neighbors of `n` in `g`

; neighbors: Node Graph -> [Maybe [List-of Node]]
; Produces `n`'s neighbors in `g`, or #false if Node `n` is not in `g`
(check-expect (neighbors 'A graph) '(B E))
(check-expect (neighbors 'F graph) '(D G))
(check-expect (neighbors 'Z graph) #false)
(define (neighbors n g)
  (local [(define maybe-assoc (assoc n g))]
    (if (false? maybe-assoc) maybe-assoc (rest maybe-assoc))))
