#lang htdp/bsl

;;; Use DrRacket to run `contains-flatt?` in this example:

(define EXAMPLE
  (cons "Fagan"
        (cons "Findler"
              (cons "Fisler"
                    (cons "Flanagan"
                          (cons "Flatt"
                                (cons "Felleisen"
                                      (cons "Friedman" '()))))))))

;;; What answer do you expect?

; contains-flatt?: List-of-names -> Boolean
; determines whether "Flatt" is in alon
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
              #true)
(check-expect (contains-flatt? (cons "A" (cons "F" (cons "C" '()))))
              #false)
(define (contains-flatt? alon)
  (cond [(empty? alon) #false]
        [(cons? alon)
         (or (string=? (first alon) "Flatt")
             (contains-flatt? (rest alon)))]))


;;; A: We expect it to be true
(check-expect (contains-flatt? EXAMPLE) #true)
