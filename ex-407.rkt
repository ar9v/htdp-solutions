#lang htdp/isl+

;;; Redesign `row-filter` using `foldr`. Once you have done so, you may merge
;;; `row-project` and `row-filter` into a single function.
;;;
;;; HINT: The `foldr` function in ISL+ may consume two lists and process them in
;;; parallel.

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

(check-expect (db-content (project school-db '("Name" "Present")))
              (db-content (make-db '("Name" "Present")
                                    '(("Alice" #true)
                                      ("Bob" #false)
                                      ("Carol" #true)
                                      ("Dave" #false)))))
(define (project db labels)
  (local [(define schema (db-schema db))
          (define content (db-content db))
          (define schema-labels (map first schema))
          (define (keep? spec) (member? (first spec) labels))
          (define (row-project row)
            (foldr (λ (cell name row) (if (member? name labels) (cons cell row) row))
                   '()
                   row
                   schema-labels))]
    (make-db (filter keep? schema)
             (map row-project content))))
