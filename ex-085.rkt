#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Define the function `run`. Given the `pre` field of an editor, it launches an
;;; interactive editor, using `render` and `edit` from the preceding two exercises for
;;; the `to-draw` and `on-key` clauses, respectively.

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

(define FONT-SIZE 16)
(define CURSOR-WIDTH 1)
(define CURSOR-HEIGHT (+ FONT-SIZE 4))
(define EDITOR-WIDTH 200)
(define EDITOR-HEIGHT CURSOR-HEIGHT)

(define X-ALIGN "left")
(define Y-ALIGN "center")
(define CURSOR-COLOR "red")
(define FONT-COLOR "black")
(define EDITOR (empty-scene EDITOR-WIDTH EDITOR-HEIGHT))
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" CURSOR-COLOR))

(define (run pre)
  (big-bang (make-editor pre "")
            [to-draw render]
            [on-key edit]))

; render: Editor -> Image
; Renders Editor `ed`'s `pre` and `post` strings as text in EDITOR, with CURSOR between
; them.
(check-expect (render (make-editor "" ""))
              (overlay/align X-ALIGN Y-ALIGN CURSOR EDITOR))

(check-expect (render (make-editor "hello" ""))
              (overlay/align
               X-ALIGN Y-ALIGN
               (beside (text-for-editor "hello") CURSOR)
               EDITOR))

(check-expect (render (make-editor "" "world"))
              (overlay/align
               X-ALIGN Y-ALIGN
               (beside CURSOR (text-for-editor "world"))
               EDITOR))

(check-expect (render (make-editor "hello" " world"))
              (overlay/align
               X-ALIGN Y-ALIGN
               (beside (text-for-editor "hello")
                       CURSOR
                       (text-for-editor " world"))
               EDITOR))

(define (render ed)
  (overlay/align
   X-ALIGN Y-ALIGN
   (beside (text-for-editor (editor-pre ed))
           CURSOR
           (text-for-editor (editor-post ed)))
   EDITOR))

; text-for-editor: String -> Image
; Renders String s as a text image with common graphical properties for `render`
(check-expect (text-for-editor "hello") (text "hello" FONT-SIZE FONT-COLOR))
(define (text-for-editor s)
  (text s FONT-SIZE FONT-COLOR))

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
