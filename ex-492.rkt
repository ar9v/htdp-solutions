#lang htdp/isl+

;;; Modify the definitions in figure 169 so that the program produces `#false`, even if
;;; it encounters the same origin twice.

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

(define a-sg
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

(define cyclic-graph
  '((A B E)
    (B E F)
    (C B D)
    (D)
    (E C F)
    (G)))

; find-path: Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'A 'D sample-graph) '(A B E C D))
(check-expect (find-path 'A 'D cyclic-graph) '(A B E C D))
(check-expect (find-path 'A 'D a-sg) #false)
(define (find-path origin d G)
  (local [(define (find-path/a o seen)
            (cond [(symbol=? o d) (list d)]
                  [(member? o seen) #false]
                  [else (local [(define next (neighbors o G))
                                (define candidate (find-path/list next (cons o seen)))]
                          (cond [(boolean? candidate) #false]
                                [else (cons o candidate)]))]))

          ; find-path/list: [List-of Node] Node Graph -> [Maybe Path]
          ; finds a path from some node on lo-Os to D
          ; if there is no path, the function produces #false
          (define (find-path/list lo-Os seen)
            (cond
              [(empty? lo-Os) #false]
              [else (local [(define candidate (find-path/a (first lo-Os) seen))]
                      (cond [(boolean? candidate) (find-path/list (rest lo-Os) seen)]
                            [else candidate]))]))]
    (find-path/a origin '())))

; neighbors: Node Graph -> [Maybe [List-of Node]]
; Produces `n`'s neighbors in `g`, or #false if Node `n` is not in `g`
(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'F sample-graph) '(D G))
(check-expect (neighbors 'Z sample-graph) #false)
(define (neighbors n g)
  (local [(define maybe-assoc (assoc n g))]
    (if (false? maybe-assoc) maybe-assoc (rest maybe-assoc))))
