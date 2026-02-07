#lang htdp/isl+

;;; Design the function `row-filter`. Construct examples for `row-filter` from the examples
;;; for `project`.
;;;
;;; Assumption: The given database passes an integrity check, meaning each row is as long
;;; as the schema and thus its list of names.

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
          (define (keep? spec) (member? (first spec) labels))
          (define (row-project row)
            (row-filter row (map first schema)))

          ; row-filter: Row [List-of Label] -> Row
          ; retains those cells whose corresponding element in `names` is also in `labels`
          ;
          ; e.g.
          ; (check-expect (row-filter '("Alice" 35 #true) '("Name" "Present"))
          ;               '("Alice" #true))
          (define (row-filter row names)
            (cond [(empty? row) '()]
                  [else (if (member (first names) labels)
                            (cons (first row) (row-filter (rest row) (rest names)))
                            (row-filter (rest row) (rest names)))]))]
    (make-db (filter keep? schema)
             (map row-project content))))
