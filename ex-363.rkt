#lang htdp/isl+

;;; All elements of Xexpr.v2 start with a Symbol, but some are followed by a list of
;;; attributes and some by just a list of Xexpr.v2s. Reformulate the definition of
;;; Xexpr.v2 to isolate the common beginning and highlight the different kinds of
;;; endings.
;;;
;;; Eliminate the use of List-of from Xexpr.v2.

; Original:
;
; An Xexpr.v2 is a list:
; -- (cons Symbol Body)
; -- (cons Symbol (cons [List-of Attribute] Body))
;
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define xexpr.v2.1
  '(machine                             ; (cons Symbol
    ((initial "red"))                   ;   (cons [List-of Attribute]
    (action                             ;         [List-of Xexpr.v2]))
     ((state "red") (next "green")))))

; New:
;
; An Xexpr.v2 is a list:
; -- (cons Symbol Body)
;
; Body is one of:
; -- '()
; -- (cons Xexpr.v2 Body)
; -- (cons Attribute* Body)
;
; Attribute* is either
; -- '()
; -- (cons Attribute Attribute*)
;
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define xexpr.v2.2
  ; Xexpr.v2 === (cons Symbol ...
  (cons 'machine
        ; Body === (cons ...
        (cons
         ; Attribute* === (cons ...
         (cons
          ; Attribute === (cons Symbol (cons String '()))
          (cons 'initial (cons "red" '()))
          ; Attribute* === '()
          '())
         ; Body === (cons ...
         (cons
          ; Xexpr.v2 === (cons Symbol ...
          (cons 'action
                ; Body === (cons ...
                (cons
                 ; Attribute* === (cons ...
                 (cons
                  ; Attribute === (cons Symbol (cons String '()))
                  (cons 'state (cons "red" '()))
                  ; Attribute* === (cons
                  (cons
                   ; Attribute === (cons Symbol (cons String '()))
                   (cons 'next (cons "green" '()))
                   ; Attribute* === '()
                   '()))
                 ; Body === '()
                 '()))
          ; Body === '()
          '()))))


(check-expect xexpr.v2.1 xexpr.v2.2)
