#lang htdp/isl+

;;; Design the function `db-union`, which consumes two databases with the exact same schema
;;; and produces a new database with this schema and the joint content of both. The
;;; function must eliminate rows with the exact same content.
;;;
;;; Assume that the schemas agree on the predicates for each column.

(define-struct db [schema content])

(define school-schema
  `(("Name" ,string?)
    ("Age" ,integer?)
    ("Present" ,boolean?)))

(define school-content-1
  '(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))

(define school-content-2
  '(("Sofia" 31 #true)
    ("Bob" 22 #true)
    ("Andres" 30 #false)
    ("Juan" 40 #false)
    ("Bob" 25 #false)))

(define school-db-1 (make-db school-schema school-content-1))
(define school-db-2 (make-db school-schema school-content-2))

; db-union: DB DB -> DB
; Produces a new DB with the contents of both `db1` and `db2`, while eliminating
; duplicates.
;
; Assumption: `db1` and `db2` have the same exact schema
(check-expect (db-content (db-union school-db-1 school-db-2))
              '(("Alice" 35 #true)
                ("Bob" 25 #false)
                ("Carol" 30 #true)
                ("Dave" 32 #false)
                ("Sofia" 31 #true)
                ("Bob" 22 #true)
                ("Andres" 30 #false)
                ("Juan" 40 #false)))
(define (db-union db1 db2)
  (local [(define original-content (db-content db1))
          (define to-join
            (filter (λ (row) (not (member? row original-content))) (db-content db2)))]
    (make-db (db-schema db1)
             (append original-content to-join))))
