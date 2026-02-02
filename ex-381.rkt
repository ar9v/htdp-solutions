#lang htdp/isl+

;;; The definitions of XMachine and X1T use quote, which is highly inappropriate for
;;; novice program designers. Rewrite them first to use `list` and then `cons`

; An XMachine is a nested list of this shape:
; (cons 'machine (cons (cons (cons 'initial (cons FSM-State '())) '()) X1T*))
;
; X1T* is one of
; -- '()
; -- (cons X1T X1T*)
;
; An X1T is a nested list of this shape:
; (cons 'action (cons X1T-Attrs '()))
;
; X1T-Attrs is a list: (cons State-Attr (cons Next-Attr '()))
; State-Attr is (cons 'state (cons FSM-State '()))
; Next-Attr is (cons 'next (cons FSM-State '()))

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

; Do _not_ try this at home. Instead, define each part separately.
(define xm0-cons
  (cons
   'machine
   (cons
    (cons (cons 'initial (cons "red" '())) '())
    (cons
     (cons
      'action
      (cons
       (cons
        (cons 'state (cons "red" '()))
        (cons (cons 'next (cons "green" '())) '()))
       '()))
     (cons
      (cons
       'action
       (cons
        (cons
         (cons 'state (cons "green" '()))
         (cons (cons 'next (cons "yellow" '())) '()))
        '()))
      (cons
       (cons
        'action
        (cons
         (cons
          (cons 'state (cons "yellow" '()))
          (cons (cons 'next (cons "red" '())) '()))
         '()))
       '())))
    )))

(check-expect xm0 xm0-cons)

; An XMachine is a nested list of this shape:
; (append (list 'machine (list (list 'initial FSM-State))) [List-of X1T])
;
; An X1T is a nested list of this shape:
; (list 'action X1T-Attrs)
;
; X1T-Attrs is a list: (list State-Attr Next-Attr)
; State-Attr is (list 'state FSM-State)
; Next-Attr is (list 'next FSM-State)

(define xm0-list
  (append
   (list
    'machine
    (list (list 'initial "red")))
   (list
    (list 'action (list (list 'state "red") (list 'next "green")))
    (list 'action (list (list 'state "green") (list 'next "yellow")))
    (list 'action (list (list 'state "yellow") (list 'next "red"))))))

(check-expect xm0 xm0-list)
