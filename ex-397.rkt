#lang htdp/isl+

;;; In a factory, employees punch time cards as they arrive in the morning and leave in the
;;; evening. Electronic time cards contain an employee number and record the number of
;;; hours per week. Employee records always contain the name of the employee, an employee
;;; number, and a pay rate.
;;;
;;; Design `wages*.v3`. The function consumes a list of employee records and a list of
;;; time-card records. It produces a list of wage records, which contain the name and
;;; weekly wage of an employee. The function signals an error if it cannot find an
;;; employee record for a time card or vice versa.
;;;
;;; Assumption: There is at most one time card per employee number

(define-struct time-card [eid h/w])
; A TimeCard is a structure
;   (make-time-card N N)
;
; interpretation: (make-time-card id h) represents an employee with an `id` that worked
; `h` hours in the week.
(define john-time (make-time-card 1 10))
(define bob-time (make-time-card 2 30))
(define carl-time (make-time-card 3 40))

(define-struct employee [name id rate])
; An Employee is a structure
;   (make-employee String N N)
;
; interpretation: (make-employee-record name id rate) represents an employee whose name is
; `name`, whose Employee Number is `id` and whose (hourly) pay rate is `rate`.
(define john (make-employee "John" 1 3))
(define bob (make-employee "Bob" 2 5))
(define carl (make-employee "Carl" 3 4))

(define-struct wage [employee amount])
; a Wage is a structure
;   (make-wage String N)
;
; interpretation: (make-wage name amount) represents the wage Employee `name` receives for
; a week's work
(define john-wage (make-wage "John" (* (employee-rate john) (time-card-h/w john-time))))
(define bob-wage (make-wage "Bob" (* (employee-rate bob) (time-card-h/w bob-time))))
(define carl-wage (make-wage "Carl" (* (employee-rate carl) (time-card-h/w carl-time))))

; wages*.v3: [List-of EmployeeRecord] [List-of TimeCard] -> [List-of WageRecord]
;
; Effect: the returned wages will be sorted by employee id.
; We could, alternatively, search for a time card per employee.
(check-expect (wages*.v3 '() '()) '())
(check-error (wages*.v3 '() (list john-wage)))
(check-error (wages*.v3 (list john) '()))
(check-expect (wages*.v3 (list john bob) (list john-time bob-time))
              (list john-wage bob-wage))
(check-error (wages*.v3 (list john) (list carl-time)))
(define (wages*.v3 es ts)
  (local [(define (wages-sorted es ts)
            (cond [(and (empty? es) (empty? ts)) '()]
                  [(and (empty? es) (cons? ts))
                   (error "No employee record for time card " (first ts))]
                  [(empty? ts)
                   (error "No time card for employee " (first es))]
                  [else
                   (cons (create-wage (first es) (first ts))
                         (wages-sorted (rest es) (rest ts)))]))]
    (wages-sorted (sort es (λ (e1 e2) (< (employee-id e1) (employee-id e2))))
                  (sort ts (λ (t1 t2) (< (time-card-eid t1) (time-card-eid t2)))))))

; create-wage: Employee TimeCard -> Wage
(check-expect (create-wage john john-time) john-wage)
(check-expect (create-wage bob bob-time) bob-wage)
(check-expect (create-wage carl carl-time) carl-wage)
(check-error (create-wage carl john-time))
(define (create-wage e t)
  (local [(define employee-record-id (employee-id e))
          (define time-card-id (time-card-eid t))]
    (if (= employee-record-id time-card-id)
        (make-wage (employee-name e) (* (employee-rate e) (time-card-h/w t)))
        (error "Error: the employee id in the record and time card don't match!"))))
