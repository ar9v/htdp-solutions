#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; As chapter 3.4 says, programs must define constants and use names intead of actual
;; constants. In this spirit, a data definition for traffic lights must use constants, too.

(define RED 0)
(define GREEN 1)
(define YELLOW 2)

; An S-TrafficLight is one of:
; -- RED
; -- GREEN
; -- YELLOW

; If the names are chosen properly, the data definition does not need an interpretation
; statement.

;; Figure 27 displays two different functions that switch the state of a traffic
;; light in a simulation program. Which of the two is properly designed using the recipe
;; for itemization? Which of the two continues to work if you change the constants to the
;; following
;;
;; (define RED "red")
;; (define GREEN "green")
;; (define YELLOW "yellow")

;;; A:
;;;
;;; Version 2 (which I almost did in ex. 59, except that I didn't use `equal?`, so
;;; my program isn't as flexible!)

(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))
