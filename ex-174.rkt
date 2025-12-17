#lang htdp/bsl

(require 2htdp/batch-io)

;;; Design a program that encodes text files numerically. Each letter in a word should be
;;; encoded as a numeric three-letter string with a value between 0 and 256. Figure 69
;;; shows our encoding function for single letters. Before you start, explain these
;;; functions.
;;;
;;; HINT (1) Use `read-words/line` to preserve the organization of the file into lines
;;; and words. (2) Read up on `explode` again.

(define FILE-PREFIX "encoded-")

; 1String -> String
; converts the given 1String to a 3-letter numeric String
;
; Explanation: Since a given code is between 0 and 256 for this problem (which corresponds
; to ASCII codes), and we want to encode letters as three-letter strings, this function
; checks the code to see if we need to pad the resulting converted value.
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a String
;
; Explanation: This takes a letter, converts it to an int (i.e. gives us its code; which
; for this problem domain corresponds to ASCII codes) and then converts that int back
; into a string representation.
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))

(define line1 (cons "The" (cons "fox" '())))
(define line2 (cons "jumps" (cons "a" (cons "dog" '()))))

(define lls (cons line1 (cons line2 '())))

; encode-file: String -> String
; Reads `filename` and represents it as a list of list of strings, in which each string
; represents each word in `filename`, encoded as a three letter string.
(define (encode-file filename)
  (write-file
   (string-append FILE-PREFIX filename)
   (collapse
    (encode-words/line (read-words/line filename)))))

; encode-words/line: LLS -> LLS
; Given a LLS, turns each word in it into a numeric 3-letter word encoded representation
(check-expect (encode-words/line '()) '())
(check-expect (encode-words/line lls)
              (cons (encode-line line1) (cons (encode-line line2) '())))
(define (encode-words/line lls)
  (cond [(empty? lls) '()]
        [(cons? lls)
         (cons (encode-line (first lls))
               (encode-words/line (rest lls)))]))

; encode-line: Line -> Line
; Given a line, returns a new line where each of its words is a numeric 3-letter encoded
; representation
(check-expect (encode-line '()) '())
(check-expect (encode-line line1)
              (cons (encode-word (explode "The")) (cons (encode-word (explode "fox")) '())))
(check-expect (encode-line line2)
              (cons (encode-word (explode "jumps"))
                    (cons (encode-word (explode "a"))
                          (cons (encode-word (explode "dog")) '()))))
(define (encode-line l)
  (cond [(empty? l) '()]
        [(cons? l)
         (cons (encode-word (explode (first l)))
               (encode-line (rest l)))]))

; encode-word: List-of-1String -> String
; Encodes a word, turning each of its letters into a numeric 3-letter representation
(check-expect (encode-word (explode "")) "")
(check-expect (encode-word (explode "The"))
              (string-append (encode-letter "T") (encode-letter "h") (encode-letter "e")))
(define (encode-word w)
  (cond [(empty? w) ""]
        [(cons? w)
         (string-append (encode-letter (first w))
                        (encode-word (rest w)))]))

; collapse: List-of-lines -> String
; Turns a list of lines into a single string, where each line is separated by the newline
; character \n
(define (collapse lol)
  (cond [(empty? lol) ""]
        [(empty? (rest lol)) (join-line (first lol))]
        [(cons? lol)
         (string-append (join-line (first lol))
                        "\n"
                        (collapse (rest lol)))]))

;;; join-line: List-of-strings -> String
;;; Joins `los` into a single string, where each word is separated by a single space.
(define (join-line los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [(cons? los) (string-append (first los) " " (join-line (rest los)))]))
