#lang htdp/isl

(require 2htdp/batch-io)

;;; Design the function `identityM`, which creates diagonal squares of 0s and 1s:
;;;
;;; > (identityM 1)
;;;   (list (list 1))
;;;
;;; > (identityM 3)
;;;   (list (list 1 0 0) (list 0 1 0) (list 0 0 1))
;;;
;;; Use the structural design recipe and exploit the power of `local`.

; identityM: Number -> [List-of [List-of Number]]
; Produces an identity matrix with `n` rows
(check-expect (identityM 1) '((1)))
(check-expect (identityM 3) '((1 0 0) (0 1 0) (0 0 1)))
(define (identityM n)
  (local (; build-row: Number -> [List-of Number]
          ; Given an "index" `i`, builds a list of size `n` in which the ith position is 1
          (define (build-row i)
            (local ((define (equals-i? x) (if (= x i) 1 0)))
              (build-list n equals-i?))))
      (build-list n build-row)))

; matrix/pp: [List-of [List-of Number]] -> [List-of Symbol]
; Pretty prints `m`
(define (matrix/pp m)
  (local ((define (row/pp r)
            (write-file 'stdout
                        (implode (append (map number->string r)
                                         (list "\n"))))))
    (map row/pp m)))
