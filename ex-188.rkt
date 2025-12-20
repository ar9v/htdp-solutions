#lang htdp/bsl+

;;; Design a program that sorts lists of emails by date
;;;
;;; Also, develop a program that sorts lists of email messages by name. To compare two
;;; strings alphabetically, use the `string<?` primitive.

(define-struct email [from date message])
; An Email Message is a structure:
;   (make-email String Number String)
;
; interpretation: (make-email f d m) represents text m, sent by f, d seconds after the
; beginning of time.

(define email1 (make-email "Robert" 100 "Hello, Matthias."))
(define email2 (make-email "Matthias" 300 "Hello, Robert."))
(define email3 (make-email "Matt" 600 "Hey, everyone."))

; An EmailProperty is one of
; -- BY-DATE
; -- BY-NAME
;
; NOTE: Sticking to separate functions is probably better :shrug:
;       But Part 3 should render this moot . . . (*spoilers*)
(define BY-DATE "date")
(define BY-NAME "name")

; sort-emails-by>: List<Email> EmailProperty -> Email
; Rearranges `emails` in descending order, by EmailProperty `prop`
(check-satisfied (sort-emails-by> '() BY-DATE) emails-sorted-by-date?)
(check-satisfied
 (sort-emails-by> (list email1 email2 email3) BY-DATE) emails-sorted-by-date?)
(check-satisfied (sort-emails-by> '() BY-NAME) emails-sorted-by-name?)
(check-satisfied
 (sort-emails-by> (list email1 email2 email3) BY-NAME) emails-sorted-by-name?)
(define (sort-emails-by> emails prop)
  (cond [(empty? emails) '()]
        [(cons? emails)
         (insert (first emails) (sort-emails-by> (rest emails) prop) prop)]))

; insert: Email List<Email> EmailProperty -> List<Email>
; Insertes `email` into sorted (descending) `emails`, using EmailProperty as the
; heuristic
(check-expect (insert email1 '() BY-DATE) (list email1))
(check-expect (insert email1 (list email3 email2) BY-DATE) (list email3 email2 email1))
(check-expect (insert email2 (list email3 email1) BY-DATE) (list email3 email2 email1))
(check-expect (insert email1 '() BY-NAME) (list email1))
(check-expect (insert email1 (list email2 email3) BY-NAME) (list email1 email2 email3))
(check-expect (insert email2 (list email1 email3) BY-NAME) (list email1 email2 email3))
(define (insert email emails prop)
  (cond [(empty? emails) (list email)]
        [(cons? emails)
         (if (email< email (first emails) prop)
             (cons (first emails) (insert email (rest emails) prop))
             (cons email emails))]))

; emails-sorted-by-date?: List<Email> -> Boolean
; Determines if `emails` is sorted in descending order, by their date
(check-expect (emails-sorted-by-date? '()) #true)
(check-expect (emails-sorted-by-date? (list email1 email2)) #false)
(check-expect (emails-sorted-by-date? (list email3 email2 email1)) #true)
(define (emails-sorted-by-date? emails)
  (cond [(or (empty? emails) (empty? (rest emails))) #true]
        [else
         (and (email> (first emails) (first (rest emails)) BY-DATE)
              (emails-sorted-by-date? (rest emails)))]))

; emails-sorted-by-name?: List<Email> -> Boolean
; Determines if `emails` is sorted in descending order, by their name
(check-expect (emails-sorted-by-name? '()) #true)
(check-expect (emails-sorted-by-name? (list email3 email2 email1)) #false)
(check-expect (emails-sorted-by-name? (list email1 email2 email3)) #true)
(define (emails-sorted-by-name? emails)
  (cond [(or (empty? emails) (empty? (rest emails))) #true]
        [else
         (and (email> (first emails) (first (rest emails)) BY-NAME)
              (emails-sorted-by-name? (rest emails)))]))

; email>: Email Email EmailProperty -> Boolean
; Determines whether `email1` is greater than `email2` by `prop`
(check-expect (email> email2 email1 BY-DATE) #true)
(check-expect (email> email2 email1 BY-NAME) #false)
(define (email> email1 email2 prop)
  (cond [(equal? prop BY-DATE) (> (email-date email1) (email-date email2))]
        [(equal? prop BY-NAME) (string>? (email-from email1) (email-from email2))]))

; email<: Email Email EmailProperty -> Boolean
; Determines whether `email1` is greater than `email2` by `prop`
(check-expect (email< email2 email1 BY-DATE) #false)
(check-expect (email< email2 email1 BY-NAME) #true)
(define (email< email1 email2 prop)
  (cond [(equal? prop BY-DATE) (< (email-date email1) (email-date email2))]
        [(equal? prop BY-NAME) (string<? (email-from email1) (email-from email2))]))
