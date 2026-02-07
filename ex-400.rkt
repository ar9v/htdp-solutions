#lang htdp/isl+

;;; Design the function `DNAprefix`. The function takes two arguments, both lists of
;;; 'a, 'c, 'g, and 't, symbols that occur in DNA descriptions. The first list is called
;;; a pattern, the second one a search string. The function returns #true if the pattern
;;; is identical to the initial part of the search string; otherwise it returns #false.

; DNAprefix: [List-of Symbol] [List-of Symbol] -> Boolean
; Returns #true if `pattern` is identical to the initial part of `search`
(check-expect (DNAprefix '() '()) #true)
(check-expect (DNAprefix '() '(a c)) #true)
(check-expect (DNAprefix '(a c) '()) #false)
(check-expect (DNAprefix '(a c) '(a c)) #true)
(check-expect (DNAprefix '(a c) '(a c g t)) #true)
(check-expect (DNAprefix '(a c) '(a g c t)) #false)
(define (DNAprefix pattern search)
  (or (empty? pattern)
      (and (cons? search)
           (equal? (first pattern) (first search))
           (DNAprefix (rest pattern) (rest search)))))

;;; Also design `DNAdelta`. This function is like `DNAprefix`, but returns the first item
;;; in the search string beyond the pattern. If the lists are identical and there is no
;;; DNA letter beyond the pattern, the function signals an error. If the pattern does not
;;; match the beginning of the search string, it returns #false. The function must not
;;; traverse either of the lists more than once.

; DNAdelta: [List-of Symbol] [List-of Symbol] -> Symbol
; Returns the first item in `search` that's beyond the pattern, if it exists.
; Signals an error otherwise
(check-error (DNAdelta '() '()))
(check-expect (DNAdelta '() '(a b)) 'a)
(check-error (DNAdelta '(a b) '()))
(check-error (DNAdelta '(a b) '(a b)))
(check-expect (DNAdelta '(a) '(a b)) 'b)
(check-error (DNAdelta '(a b) '(a)))
(check-error (DNAdelta '(a b) '(a g t)))
(define (DNAdelta pattern search)
  (cond [(empty? search) (error "There's no delta!")]
        [(empty? pattern) (first search)]
        [else
         (if (equal? (first pattern) (first search))
             (DNAdelta (rest pattern) (rest search))
             (error "Mismatch: " (first pattern) " " (first search)))]))
