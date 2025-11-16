#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Design `edit`. The function consumes two inputs, an editor `ed` and a KeyEvent `ke`,
;;; and it produces another Editor. Its task is to add a single-character KeyEvent `ke` to
;;; the end of the `pre` field of `ed`, unless `ke` denotes the backspace ("\b") key.
;;; In that case, it deletes the character immediately to the left of the cursor (if there
;;; are any). The function ignores the tab key ("\t") and the return key ("\r")
;;;
;;; The function pays attention to only two KeyEvents longer than one letter: "left" and
;;; "right". The left arrow moves the cursor one character to the left (if any), and the
;;; right arrow key moves it one character to the right (if any). All other such KeyEvents
;;; are ignored.
;;;
;;; Develop a goodly number of examples for `edit`, paying attention to special cases.
;;; When we solved this exercise, we created 20 examples and turned all of them into tests.
;;;
;;;
;;; HINT: Think of this function as consuming KeyEvents, a collection that is specified
;;; by an enumeration. It uses auxiliary functions to deal with the Editor structure.
;;; Keep a wish list handy; you will need to design additional functions for most of
;;; these auxiliary functions, such as `string-first`, `string-rest`, `string-last`, and
;;; `string-remove-last`.

(define-struct editor [pre post])
; An Editor is a structure:
;  (make-editor String String)
;
; interpretation: (make-editor s t) describes an editor whose
; visible text is (string-append s t) with the cursor displayed
; between s and t

(define empty-ed (make-editor "" ""))
(define cursor-at-beg (make-editor "" "hello"))
(define cursor-in-mid (make-editor "hello" " world"))
(define cursor-at-end (make-editor "hello" ""))

; edit: Editor KeyEvent -> Editor
; Produces a new Editor, resulting from updating `ed` with `ke`.
(check-expect (edit empty-ed "left") empty-ed)
(check-expect (edit empty-ed "right") empty-ed)
(check-expect (edit empty-ed "\b") empty-ed)
(check-expect (edit empty-ed "a") (make-editor "a" ""))
(check-expect (edit empty-ed "\r") empty-ed)

(check-expect (edit cursor-at-beg "left") cursor-at-beg)
(check-expect (edit cursor-at-beg "right") (make-editor "h" "ello"))
(check-expect (edit cursor-at-beg "\b") cursor-at-beg)
(check-expect (edit cursor-at-beg "a") (make-editor "a" "hello"))
(check-expect (edit cursor-at-beg "\r") cursor-at-beg)

(check-expect (edit cursor-in-mid "left") (make-editor "hell" "o world"))
(check-expect (edit cursor-in-mid "right") (make-editor "hello " "world"))
(check-expect (edit cursor-in-mid "\b") (make-editor "hell" " world"))
(check-expect (edit cursor-in-mid "w") (make-editor "hellow" " world"))
(check-expect (edit cursor-in-mid "\r") cursor-in-mid)

(check-expect (edit cursor-at-end "left") (make-editor "hell" "o"))
(check-expect (edit cursor-at-end "right") cursor-at-end)
(check-expect (edit cursor-at-end "\b") (make-editor "hell" ""))
(check-expect (edit cursor-at-end "w") (make-editor "hellow" ""))
(check-expect (edit cursor-at-end "\r") cursor-at-end)

(define (edit ed ke)
  (cond [(key=? ke "left")
         (make-editor (string-remove-last (editor-pre ed))
                      (string-append (string-last (editor-pre ed))
                                     (editor-post ed)))]
        [(key=? ke "right")
         (make-editor (string-append (editor-pre ed)
                                     (string-first (editor-post ed)))
                      (string-rest (editor-post ed)))]
        [(key=? ke "\b")
         (make-editor (string-remove-last (editor-pre ed))
                      (editor-post ed))]
        [(and (1string? ke) (not (escaped? ke)))
         (make-editor (string-append (editor-pre ed) ke)
                      (editor-post ed))]
        [else ed]))

; string-remove-last: String -> String
; Given a String `s`, return a new string that is `s` w/o its last character.
; If `s` is empty, return `s`.
;
(check-expect (string-remove-last "abc") "ab")
(check-expect (string-remove-last "b") "")
(check-expect (string-remove-last "") "")
(define (string-remove-last s)
  (if (string=? s "")
      ""
      (substring s 0 (sub1 (string-length s)))))

; string-last: String -> 1String
; Returns the last character of `s`, the given string
;
(check-expect (string-last "a") "a")
(check-expect (string-last "31") "1")
(check-expect (string-last "c0rr3ct h0rs3, b4tt3ry 5t4pl3!") "!")
(check-expect (string-last "") "")
(define (string-last s)
  (if (string=? s "") "" (string-ith s (sub1 (string-length s)))))

; string-first: String -> 1String
; Returns the first character of `s`, the given string.
;
(check-expect (string-first (string-first "a")) "a")
(check-expect (string-first (string-first "31")) "3")
(check-expect (string-first (string-first "c0rr3ct! h0r53 b4tt3ry, 5t4pl3")) "c")
(check-expect (string-first "") "")
(define (string-first s)
  (if (string=? s "") "" (string-ith s 0)))

; string-rest: String -> String
; Given a string `s`, returns a string which is `s` except for the first character.
; If `s` is empty, `s` is returned.
;
(check-expect (string-rest "abc") "bc")
(check-expect (string-rest "b") "")
(check-expect (string-rest "") "")
(define (string-rest s)
  (if (string=? s "")
      ""
      (substring s 1)))

; 1string?: String -> Boolean
; Checks if this is a 1String
(check-expect (1string? "\r") #true)
(check-expect (1string? "\b") #true)
(check-expect (1string? "\t") #true)
(check-expect (1string? "!") #true)
(check-expect (1string? "cat") #false)
(define (1string? s)
  (= (string-length s) 1))

; escaped?: 1String -> Boolean
; Checks if the given 1String is an escape sequence
(check-expect (escaped? "\r") #true)
(check-expect (escaped? "\b") #true)
(check-expect (escaped? "\t") #true)
(check-expect (escaped? "!") #false)
(define (escaped? os)
  (or (key=? os "\r")
      (key=? os "\b")
      (key=? os "\t")))
