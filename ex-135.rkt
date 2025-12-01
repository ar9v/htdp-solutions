#lang htdp/bsl

(define (contains-flatt? alon)
  (cond [(empty? alon) #false]
        [(cons? alon)
         (or (string=? (first alon) "Flatt")
             (contains-flatt? (rest alon)))]))

;;; Use DrRacket's stepper to check the calculation for
;;; (contains-flatt? (cons "Flatt" (cons "C" '())))
;;;
;;; (This is what 8.4 shows in figures 48 through 50)

;;; Also use the stepper to determine the value of
(contains-flatt?
 (cons "A" (cons "Flatt" (cons "C" '()))))

;; (contains-flatt?
;;  (cons "A" (cons "Flatt" (cons "C" '()))))
;; ==
;; (cond [(empty? (cons "A" (cons "Flatt" (cons "C" '())))) #false]
;;       [(cons? (cons "A" (cons "Flatt" (cons "C" '()))))
;;        (or (string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt")
;;            (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))])
;; ==
;; (cond [#false #false]
;;       [(cons? (cons "A" (cons "Flatt" (cons "C" '()))))
;;        (or (string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt")
;;            (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))])
;; ==
;; (cond [(cons? (cons "A" (cons "Flatt" (cons "C" '()))))
;;        (or (string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt")
;;            (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))])
;; ==
;; (cond [#true
;;        (or (string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt")
;;            (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))])
;; ==
;; (or (string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt")
;;     (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or (string=? "A" "Flatt")
;;     (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or #false
;;     (contains-flatt? (rest (cons "A" (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or #false
;;     (contains-flatt? (cons "Flatt" (cons "C" '()))))
;; ==
;; (or #false
;;     (cond [(empty? (cons "Flatt" (cons "C" '()))) #false]
;;           [(cons? (cons "Flatt" (cons "C" '())))
;;            (or (string=? (first (cons "Flatt" (cons "C" '()))) "Flatt")
;;                (contains-flatt? (rest (cons "Flatt" (cons "C" '())))))]))
;; ==
;; (or #false
;;     (cond [#false #false]
;;           [(cons? (cons "Flatt" (cons "C" '())))
;;            (or (string=? (first (cons "Flatt" (cons "C" '()))) "Flatt")
;;                (contains-flatt? (rest (cons "Flatt" (cons "C" '())))))]))
;; ==
;; (or #false
;;     (cond [(cons? (cons "Flatt" (cons "C" '())))
;;            (or (string=? (first (cons "Flatt" (cons "C" '()))) "Flatt")
;;                (contains-flatt? (rest (cons "Flatt" (cons "C" '())))))]))
;; ==
;; (or #false
;;     (cond [#true
;;            (or (string=? (first (cons "Flatt" (cons "C" '()))) "Flatt")
;;                (contains-flatt? (rest (cons "Flatt" (cons "C" '())))))]))
;; ==
;; (or #false
;;     (or (string=? (first (cons "Flatt" (cons "C" '()))) "Flatt")
;;         (contains-flatt? (rest (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or #false
;;     (or (string=? "Flatt" "Flatt")
;;         (contains-flatt? (rest (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or #false
;;     (or #true
;;         (contains-flatt? (rest (cons "Flatt" (cons "C" '()))))))
;; ==
;; (or #false
;;     #true)
;; ==
;; #true
