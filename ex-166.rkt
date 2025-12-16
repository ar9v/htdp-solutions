#lang htdp/bsl

;;; The `wage*.v2` function consumes a list of work records and produces a list of numbers.
;;; Of course, functions may also produce lists of structures.
;;;
;;; Develop a data representation for paychecks. Assume that a paycheck contains two
;;; distinctive pieces of information: the employee's name and an amount. Then design the
;;; function `wage*.v3`. It consumes a list of work records and computes a list of
;;; paychecks from it, one per record.

(define-struct work [employee rate hours])
; A (piece of) Work is a structue:
;   (make-work String Number Number)
;
; interpretation: (make-work n r h) combines the name, with the pay rate r and the
; number of hours h

; Low (short for List of works) is one of:
; -- '()
; -- (cons Work Low)
;
; interpretation: an instance of Low represents the hours worked for a number of employees

(define low-ex-1 '())
(define low-ex-2 (cons (make-work "Robby" 11.95 39)
                             '()))

(define-struct paycheck [name amount])
; A Paycheck is a structure:
;   (make-paycheck String Number)
;
; interpretation: (make-paycheck name amount) represents how `name` being paid `amount`

; wage*.v3: Low -> List-of-Paycheck
; computes the weekly wages for a list of work records, returned as paychecks
(check-expect (wage*.v3 low-ex-1) '())
(check-expect (wage*.v3 low-ex-2) (cons (make-paycheck "Robby" (* 11.95 39)) '()))
(define (wage*.v3 low)
  (cond [(empty? low) '()]
        [(cons? low)
         (cons (wage.v3 (first low))
               (wage*.v3 (rest low)))]))

; wage.v3: Work -> Paycheck
; computes the wage for the given work record `w`, returned as a paycheck
(check-expect (wage.v3 (first low-ex-2)) (make-paycheck "Robby" (* 11.95 39)))
(define (wage.v3 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))


;;; In reality, a paycheck also contains an employee number. Develop a data representation
;;; for employee information and change the data definition for work records so that it
;;; uses employee information and not just a string for the employee's name. Also change
;;; your data representation of paychecks so that it contains an employee's name and number
;;; too. Finally, design `wage*.v4`, a function that maps lists of revised work records to
;;; lists of revised paychecks

(define-struct employee [id name])
; An Employee is a structure
;   (make-employee Integer String)
;
; interpretation: (make-employee id name) represents an employee's info

(define employee-1 (make-employee 1 "Matthew"))
(define employee-2 (make-employee 2 "Robby"))

(define-struct work-v2 [employee rate hours])
; A WorkV2 is a structue:
;   (make-work-v2 Employee Number Number)
;
; interpretation: (make-work-v2 n r h) combines the employee, with the pay rate r and the
; number of hours h

(define work-ex-1 (make-work-v2 employee-1 12.95 45))
(define work-ex-2 (make-work-v2 employee-2 11.95 39))

(define-struct paycheck-v2 [employee amount])
; A PaycheckV2 is a structure:
;   (make-paycheck-v2 String Number)
;
; interpretation: (make-paycheck-v2 name amount) represents how `name` being paid `amount`

(define paycheck-1 (make-paycheck-v2 employee-1 (* 12.95 45)))
(define paycheck-2 (make-paycheck-v2 employee-2 (* 11.95 39)))

; wage*.v4: List-of-WorkV2 -> List-of-PaycheckV2
; Computes the paychecks for all work records `low`
(check-expect (wage*.v4 '()) '())
(check-expect (wage*.v4 (cons work-ex-2 '())) (cons paycheck-2 '()))
(define (wage*.v4 low)
  (cond [(empty? low) '()]
        [(cons? low)
         (cons (wage.v4 (first low))
               (wage*.v4 (rest low)))]))

; wage.v4: WorkV2 -> PaycheckV2
; Computes the paycheck corresponding to work record `w`
(check-expect (wage.v4 work-ex-1) paycheck-1)
(check-expect (wage.v4 work-ex-2) paycheck-2)
(define (wage.v4 w)
  (make-paycheck-v2 (work-v2-employee w)
                    (* (work-v2-rate w) (work-v2-hours w))))
