#lang htdp/bsl+

;;; Eliminate `quote` in favor of `list` from these expressions:

; '(1 "a" 2 #false 3 "c")
(check-expect '(1 "a" 2 #false 3 "c")
              (list '1 '"a" '2 '#false '3 '"c"))
(check-expect '(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

; '()
(check-expect '() (list))

; '(("alan" 1000)
;   ("barb" 2000)
;   ("carl" 1500))
(check-expect
 '(("alan" 1000) ("barb" 2000) ("carl" 1500))
 (list '("alan" 1000) '("barb" 2000) '("carl" 1500)))

(check-expect
 '(("alan" 1000) ("barb" 2000) ("carl" 1500))
 (list (list '"alan" '1000) (list '"barb" '2000) (list '"carl" '1500)))

(check-expect
 '(("alan" 1000) ("barb" 2000) ("carl" 1500))
 (list (list "alan" 1000) (list "barb" 2000) (list "carl" 1500)))

;;; Now eliminate `list` in favor of `cons` where needed.
; '(1 "a" 2 #false 3 "c")
(check-expect (list 1 "a" 2 #false 3 "c")
              (cons 1 (cons "a" (cons 2 (cons #false (cons 3 (cons "c" '())))))))

; '()
(check-expect (list) '()) ; nothing else to do here

; '(("alan" 1000)
;   ("barb" 2000)
;   ("carl" 1500))
(check-expect
 (list (list "alan" 1000) (list "barb" 2000) (list "carl" 1500))
 (cons
  (cons "alan" (cons 1000 '()))
  (cons
   (cons "barb" (cons 2000 '()))
   (cons
    (cons "carl" (cons 1500 '()))
    '()))))
