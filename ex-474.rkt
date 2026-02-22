#lang htdp/isl+

;;; Redesign the `find-path` program as a single function

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
  (local [(define (find-path/list lo D G)
            (cond [(empty? lo) #false]
                  [else
                   (local [(define candidate (find-path (first lo) D G))]
                     (cond [(boolean? candidate) (find-path/list (rest lo) D G)]
                           [else candidate]))]))
          (define (neighbors n g)
            (local [(define maybe-assoc (assoc n g))]
              (if (false? maybe-assoc) maybe-assoc (rest maybe-assoc))))]
    (cond [(symbol=? origination destination) (list destination)]
          [else (local [(define next (neighbors origination G))
                        (define candidate (find-path/list next destination G))]
                  (cond [(boolean? candidate) #false]
                        [(cons? candidate) (cons origination candidate)]))])))
