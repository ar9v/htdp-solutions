#lang htdp/bsl

;;; Spell out the laws for these structure type definitions

(define-struct centry [name home office cell])
;;; (centry-name (make-centry n h o c))   == n
;;; (centry-home (make-centry n h o c))   == h
;;; (centry-office (make-centry n h o c)) == o
;;; (centry-cell (make-centry n h o c))   == c


(define-struct phone [area number])
;;; (phone-area (make-phone a n))   == a
;;; (phone-number (make-phone a n)) == n

;;; Use DrRacket's stepper to confirm 101 as the value of this expression

(phone-area
 (centry-office
  (make-centry "Shriram Fisler"
               (make-phone 207 "363-2421")
               (make-phone 101 "776-1099")
               (make-phone 208 "112-9981"))))

;; (phone-area
;;  (centry-office
;;   (make-centry "Shriram Fisler"
;;                (make-phone 207 "363-2421")
;;                (make-phone 101 "776-1099")
;;                (make-phone 208 "112-9981"))))
;; ==
;; (phone-area
;;  (make-phone 101 "776-1099"))
;; ==
;; 101
