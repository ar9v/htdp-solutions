#lang htdp/bsl

;; Boolean expressions can express some everyday problems.
;; Suppose you want to decide whether today is an appropriate day to go to mall.
;; You go to the mall either if it is not sunny or if today is Friday
;; (because that is when stores post new sales items).
;;
;; Here is how you could go about it using your new knowledge about
;; Booleans. First add these two lines to the definitions area of DrRacket:
(define sunny #true)
(define friday #false)

;; Now create an expression that computes whether sunny is false or friday is true.
(or (not sunny) friday)

;; So in this particular case, the answer is #false. (Why?)
;;
;; Answer:
;; (or (not sunny) friday)
;; ===
;; (or (not true) friday)
;; ===
;; (or false friday)
;; ===
;; (or false false)
;; ===
;; false

;; See exercise 1 for how to create expressions in DrRacket. How many
;; combinations of Booleans can you associate with sunny and friday?
;;
;; Answer: Four
;;
;; sunny = false, friday = false
;; sunny = false, friday = true
;; sunny = true, friday = false
;; sunny = true, friday = true
