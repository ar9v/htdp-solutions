#lang htdp/bsl

(require 2htdp/batch-io)

;;; Design a program that removes all articles from a text file. The program consumes the
;;; name `n` of a file, reads the file, removes the articles, and writes the result out
;;; to a file whose name is the result of concatenating "no-articles" with `n`. For this
;;; exercise, an article is one of the following three words: "a", "an", and "the".
;;;
;;; Use `read-words/line` so that the transformation retains the organization of the
;;; original text into lines and words. When the program is designed, run it on the
;;; Piet Hein poem.

(define ARTICLES (list "a" "an" "the"))
(define FILE-PREFIX "no-articles-")

(define line1 (cons "The" (cons "quick" (cons "brown" (cons "fox" '())))))
(define line2 (cons "jumps" (cons "over" (cons "a" (cons "lazy" (cons "dog" '()))))))

(define lls (cons line1 (cons line2 '())))

; remove-articles: String -> String
; Removes all articles in file `filename` and creates a new file, `{filename}-no-articles`
(define (remove-articles filename)
  (write-file
   (string-append FILE-PREFIX filename)
   (collapse (remove-articles/lls (read-words/line filename)))))

; remove-articles/lls: LLS -> LLS
; Removes all articles from lls
(check-expect (remove-articles/lls '()) '())
(check-expect (remove-articles/lls lls)
              (cons (remove-articles/line line1)
                    (cons (remove-articles/line line2) '())))
(define (remove-articles/lls lls)
  (cond [(empty? lls) '()]
        [(cons? lls)
         (cons (remove-articles/line (first lls))
               (remove-articles/lls (rest lls)))]))

; remove-articles/line: Line -> Line
(check-expect (remove-articles/line line1)
              (cons "quick" (cons "brown" (cons "fox" '()))))
(check-expect (remove-articles/line line2)
              (cons "jumps" (cons "over" (cons "lazy" (cons "dog" '())))))
(define (remove-articles/line l)
  (cond [(empty? l) '()]
        [(cons? l)
         (if (article? (first l))
             (remove-articles/line (rest l))
             (cons (first l) (remove-articles/line (rest l))))]))

; article?: String -> Boolean
; True if the given string is an ARTICLE
(check-expect (article? "a") #true)
(check-expect (article? "an") #true)
(check-expect (article? "AN") #true)
(check-expect (article? "the") #true)
(check-expect (article? "The") #true)
(check-expect (article? "quick") #false)
(define (article? s) (member? (string-downcase s) ARTICLES))

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
