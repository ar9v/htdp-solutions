#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `reorder`. The function consumes a database `db` and a list `lol` of Labels.
;;; It produces a database like `db` but with its columns reordered according to `lol`.
;;;
;;; HINT: Read up on `list-ref`
;;;
;;; At first assume that `lol` consists exactly of the labels of `db`'s columns.
;;; Once you have completed the design, study what has to be changed if `lol` contains
;;; fewer labels than there are columns and strings that are not labels of a column in
;;; `db`.

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

(define reordered-schema
  `(("Present" ,boolean?)
    ("Age" ,integer?)
    ("Name" ,string?)))

(define reordered-content
  '((#true 35 "Alice")
    (#false 25 "Bob")
    (#true 30 "Carol")
    (#false 32 "Dave")))

(define reordered-db (make-db reordered-schema reordered-content))

; reorder: DB [List-of Label] -> DB
; Produces a DB like `db`, but where each rows' cells are reordered as specified by `lol`
(check-expect (db-content (reorder school-db '("Present" "Age" "Name")))
              (db-content reordered-db))
(check-expect (map first (db-schema (reorder school-db '("Present" "Age" "Name"))))
              (map first (db-schema reordered-db)))
(check-expect (db-content (reorder school-db '("Present" "Name")))
              '((#true "Alice")
                (#false "Bob")
                (#true "Carol")
                (#false "Dave")))
(check-error (reorder school-db '("Present" "Foo")))
(check-error (reorder school-db '("Present" "Age" "Name" "Foo")))
(define (reorder db lol)
  (local [(define (reorder-row r)
            (for/list [(l lol)]
              (local [(define label-index (assoc l schema-indices))]
                (if (not (false? label-index))
                    (list-ref r (second label-index))
                    (error "Error: could not find label " l " in the schema")))))
          (define (indices schema-labels)
            (build-list (length schema-labels) (λ (n) (list (list-ref schema-labels n) n))))
          (define schema-indices (indices (map first (db-schema db))))]
    (make-db (for/list [(l lol)] (assoc l (db-schema db)))
             (for/list [(row (db-content db))] (reorder-row row)))))
