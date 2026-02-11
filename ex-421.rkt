#lang htdp/isl+

;;; Is `(bundle '("a" "b" "c") 0)` a proper use of the `bundle` function? What does it
;;; produce? Why?
;;;
;;; A:
;;; It's not a "proper" use inasmuch as it doesn't make sense to bundle 1Strings into
;;; 0-sized chunks, but also, it loops forever! Since we call `(drop s n)` with 0, the
;;; input to our recursive call remains the same, and therefore never exercises the
;;; first branch of the `cond` expression.

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

; (bundle '("a" "b" "c") 0)
