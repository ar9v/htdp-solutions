#lang htdp/isl+

;;; Design the function `select`. It consumes a database, a list of labels, and a predicate
;;; on rows. The result is a list of rows that satisfy the given predicate, projected down
;;; to the set of labels.

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

(define school-db (make-db school-schema school-content))

; select: DB [List-of Label] [Row -> Boolean] -> DB
; Produces a DB like `db` but only with rows that satisfy predicate `where`, and projected
; to the labels in `lol`.
;
; Assumption: `lol` contains labels that match the schema's (mostly informed by the fact
; that the book's `project` also foregoes handling the opposite case)
(check-expect (db-content (select school-db
                                  '("Name")
                                  (λ (row) (<= (second row) 30))))
              (db-content
               (make-db '("Name") '(("Bob") ("Carol")))))
(define (select db lol where)
  (project (make-db (db-schema db) (filter where (db-content db)))
           lol))

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define (keep? c) (member? (first c) labels))
          (define mask (map keep? schema))
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask)))
    (make-db (filter keep? schema) (map row-project content))))
