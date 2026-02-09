#lang htdp/isl+

(require 2htdp/abstraction)

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

(define presence-schema-extra
  `(("Present" ,boolean?)
    ("Description" ,string?)
    ("Integer" ,number?)
    ("Symbol" ,symbol?)))

(define presence-content
  '((#true "presence")
    (#false "absence")))

(define presence-content-extra
  '((#true "presence" 1 true)
    (#false "absence" 0 false)))

(define presence-content-doubles
  '((#true "presence")
    (#true "here")
    (#false "absence")
    (#false "there")))

(define school-db (make-db school-schema school-content))
(define presence-db (make-db presence-schema presence-content))
(define presence-extra-db (make-db presence-schema-extra presence-content-extra))
(define presence-doubles-db (make-db presence-schema presence-content-doubles))

(define joined-db
  (make-db (append (db-schema school-db) (rest (db-schema presence-db)))
           '(("Alice" 35 "presence")
             ("Bob" 25 "absence")
             ("Carol" 30 "presence")
             ("Dave" 32 "absence"))))

(define joined-extra-db
  (make-db (append (db-schema school-db) (rest (db-schema presence-extra-db)))
           '(("Alice" 35 "presence" 1 true)
             ("Bob" 25 "absence" 0 false)
             ("Carol" 30 "presence" 1 true)
             ("Dave" 32 "absence" 0 false))))

(define joined-doubles-db
  (make-db (append (db-schema school-db) (rest (db-schema presence-doubles-db)))
           '(("Alice" 35 "presence")
             ("Alice" 35 "here")
             ("Bob" 25 "absence")
             ("Bob" 25 "there")
             ("Carol" 30 "presence")
             ("Carol" 30 "here")
             ("Dave" 32 "absence")
             ("Dave" 32 "there"))))

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
(check-expect (map first (db-schema (join school-db presence-extra-db)))
              (map first (db-schema joined-extra-db)))
(check-expect (db-content (join school-db presence-extra-db))
              (db-content joined-extra-db))
(check-expect (db-content (join school-db presence-doubles-db))
              (db-content joined-doubles-db))
(define (join db1 db2)
  (local [(define (translate-column row)
            (map (λ (found-row) (append (but-last row) (rest found-row)))
                 (assoc* (last row) (db-content db2))))]
    (make-db (append (db-schema db1) (rest (db-schema db2)))
             (foldr append '() (map translate-column (db-content db1))))))

; assoc*: X [AList X Y] -> [List-of [Association X Y]]
; Given `x` and an `alist`, returns a list of all associations for which
; `x` is the key, if any.
(define (assoc* x alist)
  (match (assoc x alist)
    [#false '()]
    [a (cons a (assoc* x (remove a alist)))]))

; but-last: [List-of X] -> [List-of X]
; Returns a list with all elements of `l` save for the last one.
(define (but-last l)
  (cond [(or (empty? l) (empty? (rest l))) '()]
        [else (cons (first l) (but-last (rest l)))]))

; last: [NonEmptyList-of X] -> X
; Returns the last element in `l`, if any.
(define (last l)
  (cond [(empty? (rest l)) (first l)]
        [else (last (rest l))]))
