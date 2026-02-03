#lang htdp/isl+

;;; In the real world, `wages*.v2` consumes lists of employee structures and lists of
;;; work records. An employee structure contains an employee's name, ssn, and pay rate.
;;; A work record also contains an an employee's name and the number of hours worked in
;;; a week. The result is a list of structures that contain the name of the employee and
;;; a weekly wage.
;;;
;;; Modify the program in this section so that it works on these realistic versions of
;;; data. Provide the necessary structure type definitions and data definitions. Use the
;;; design recipe to guide the modification process.

(define-struct employee [name ssn pay-rate])
; An Employee is a structure
;   (make-employee String String Number)
;
; interpretation: (make-employee n s rate) represents an employee named `n` with SSN `s`,
; whose hourly pay rate is `rate`
(define john (make-employee "John" "1" 1))
(define bob (make-employee "Bob" "2" 2))

(define-struct work-record [employee-name hours-worked])
; A WorkRecord is a structure
;   (make-work-record String Number)
;
; interpretation: (make-work-record name hours) represents a record that states that
; the employee whose name is `name` worked a given amount of `hours` in a week.
(define john-record (make-work-record "John" 5))
(define bob-record (make-work-record "Bob" 3))

(define-struct wage [employee amount])
; A Wage is a structure
;   (make-wage String Number)
;
; interpretation: (make-wage name amount) represents a record that states that an employee
; named `name` made a given `amount` in the week.
(define john-wage (make-wage "John" 5))
(define bob-wage (make-wage "Bob" 6))

; wages*: [List-of Employee] [List-of WorkRecord] -> [List-of Wage]
;
; Assumptions:
; -- `employees` and `records` are the same size
; -- For each `employee` element, there's a `record` element with the same name
(check-satisfied (wages* (list john bob) (list bob-record john-record))
                 (λ (res)
                   (and (member? john-wage res)
                        (member? bob-wage res))))
(check-satisfied (wages* (list john bob) (list john-record bob-record))
                 (λ (res)
                   (and (member? john-wage res)
                        (member? bob-wage res))))
(define (wages* employees records)
  (local [(define (wages es rs)
            (cond
              [(empty? es) '()]
              [else
               (cons
                (weekly-wage (first es) (first rs))
                (wages (rest es) (rest rs)))]))
          (define sorted-es
            (sort employees (λ (e1 e2) (string<? (employee-name e1) (employee-name e2)))))
          (define sorted-rs
            (sort records (λ (r1 r2) (string<? (work-record-employee-name r1)
                                               (work-record-employee-name r2)))))]
    (wages sorted-es sorted-rs)))

; weekly-wage: Employee WorkRecord -> Wage
; computes the weekly wage from an Employee's pay-rate and, a WorkRecord's hours
(check-expect (weekly-wage john john-record) (make-wage "John" 5))
(check-expect (weekly-wage bob bob-record) (make-wage "Bob" 6))
(define (weekly-wage e r)
  (make-wage (employee-name e)
             (* (employee-pay-rate e) (work-record-hours-worked r))))
