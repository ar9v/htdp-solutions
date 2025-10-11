#lang htdp/bsl

;; Use the same setup as in exercise 3 to create an expression that
;; deletes the ith position from `str`    
(define str "helloworld")
(define i 5)


;; Then create an expression using string primitives that adds "_" at the position `i`.
(string=?
 (string-append (substring str 0 i)
                (substring str (add1 i)))
 "helloorld")

;; Clearly this expression creates a shorter string than the given one.
;; Which values for i are legitimate?
;;
;; A: [0, (string-length str))
;;    which in this case is [0, 10)
