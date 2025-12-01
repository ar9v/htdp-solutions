#lang htdp/bsl

(define-struct pair [left right])
; A ConsPair is a structure:
; (make-pair Any Any).

; A ConsOrEmpty is one of:
; -- '()
; -- (make-pair Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all lists
; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument ...")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "cannot be given an empty list")
      (pair-left a-list)))

; ConsOrEmpty -> ConsOrEmpty
(define (our-rest a-list)
  (if (empty? a-list)
      (error 'our-rest "cannot be given an empty list")
      (pair-right a-list)))

;;; Validate with DrRacket's stepper
(our-first (our-cons "a" '()))

;; (our-first (our-cons "a" '()))
;; ==
;; (our-first (cond [(empty? '()) (make-pair "a" '())]
;;                  [(pair? '()) (make-pair "a" '())]
;;                  [else (error "cons: second argument ...")]))
;; ==
;; (our-first (cond [#true (make-pair "a" '())]
;;                  [(pair? '()) (make-pair "a" '())]
;;                  [else (error "cons: second argument ...")]))
;; ==
;; (our-first (make-pair "a" '()))
;; ==
;; (if (empty? (make-pair "a" '()))
;;     (error 'our-first "cannot be given an empty list")
;;     (pair-left (make-pair "a" '())))
;; ==
;; (if #false
;;     (error 'our-first "cannot be given an empty list")
;;     (pair-left (make-pair "a" '())))
;; ==
;; (pair-left (make-pair "a" '()))
;; ==
;; "a"

;;; For brevity, I'll skip over `our-cons`, since it's the same as above
(our-rest (our-cons "a" '()))

;; (our-rest (our-cons "a" '()))
;; ==
;; (our-rest (make-pair "a" '()))
;; ==
;; (if (empty? (make-pair "a" '()))
;;     (error 'our-rest "cannot be given an empty list")
;;     (pair-right (make-pair "a" '())))
;; ==
;; (if #false
;;     (error 'our-rest "cannot be given an empty list")
;;     (pair-right (make-pair "a" '())))
;; ==
;; (pair-right (make-pair "a" '()))
;; ==
;; '()
