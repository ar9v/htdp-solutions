#lang htdp/isl+

;;; Design `join`, a function that consumes two databases: `db-1` and `db-2`. The schema
;;; of `db-2` starts with the exact same spec that the schema of `db-1` ends in. The
;;; function creates a database from `db-1` by replacing the last cell in each row with
;;; the *translation* of the cell in `db-2`.

(define-struct db [schema content])

(define school-schema
  `(("Name" ,string?)
    ("Age" ,integer?)
    ("Present" ,boolean?)))

(define school-content
  '(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))

(define presence-schema
  `(("Present" ,boolean?)
    ("Description" ,string?)))

(define presence-content
  '((#true "presence")
    (#false "absence")))

(define presence-db (make-db presence-schema presence-content))
(define school-db (make-db school-schema school-content))

(define joined-db
  (make-db (append (db-schema school-db) (rest (db-schema presence-db)))
           '(("Alice" 35 "presence")
             ("Bob" 25 "absence")
             ("Carol" 30 "presence")
             ("Dave" 32 "absence"))))

; join: DB DB -> DB
; Given two databases, produces a database where the schema is the union of
; both schemas (except the shared Spec), and where each row contains info
; from both databases.
;
; Constraints:
; -- `db2`'s first spec is the same as `db1`'s last spec
(check-expect (map first (db-schema (join school-db presence-db)))
              (map first (db-schema joined-db)))
(check-expect (db-content (join school-db presence-db))
              (db-content joined-db))
(define (join db1 db2)
  (local [(define (translate-column row)
            (append (but-last row)
                    (rest (assoc (last row) (db-content db2)))))
          (define (but-last l)
            (cond [(empty? (rest l)) '()]
                  [else (cons (first l) (but-last (rest l)))]))
          (define (last l)
            (cond [(empty? (rest l)) (first l)]
                  [else (last (rest l))]))]
    (make-db (append (db-schema db1) (rest (db-schema db2)))
             (map translate-column (db-content db1)))))
