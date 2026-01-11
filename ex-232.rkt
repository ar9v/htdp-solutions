#lang htdp/bsl+

;;; Eliminate `quasiquote` and `unquote` from the following expressions so that they are
;;; written with `list` instead. Also write down the nested lists that these expressions
;;; produce.

; `(1 "a" 2 #false 3 "c")
(check-expect `(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

; `(("alan" ,(* 2 500))
;   ("barb" 2000)
;   (,(string-append "carl" " , the great") 1500)
;   ("dawn" 2300))
(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,(string-append "carl" " , the great") 1500)
                ("dawn" 2300))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl , the great" 1500)
                    (list "dawn" 2300)))

(define title "ratings")
; `(html
;   (head
;    (title ,title))
;   (body
;    (h1 ,title)
;    (p "A second web page")))
(check-expect `(html
                (head
                 (title ,title))
                (body
                 (h1 ,title)
                 (p "A second web page")))
              (list `html
                    (list `head (list `title title))
                    (list `body
                          (list `h1 title)
                          (list `p "A second web page"))))
