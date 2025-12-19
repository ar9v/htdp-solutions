#lang htdp/bsl

;;; Design the functions:
;;;
;;; -- editor-lft
;;; -- editor-rgt
;;; -- editor-del

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
;
; An Lo1S is one of
; -- '()
; -- (cons 1String Lo1S)
;
; interpretation: (make-editor pre post) represents an editor where pre is the
; text before the cursor (reversed), and post is the text after the cursor.

; create-editor: String String -> Editor
; Create an Editor from `pre` and `post`
(check-expect (create-editor "" "") (make-editor '() '()))
(check-expect (create-editor "foo" "bar") (make-editor (reverse (explode "foo"))
                                                       (explode "bar")))
(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))

(define empty-editor (create-editor "" ""))
(define cursor-beginning (create-editor "" "foo"))
(define cursor-end (create-editor "foo" ""))
(define cursor-mid (create-editor "foo" "bar"))


; main: String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
            [to-draw editor-render]
            [on-key editor-kh]))

; editor-render: Editor -> Image
; renders an editor as an image of the two texts separated by the cursor
(define (editor-render e) MT)

; editor-kh: Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh empty-editor "e") (create-editor "e" ""))
(check-expect (editor-kh empty-editor "\b") empty-editor)
(check-expect (editor-kh empty-editor "left") empty-editor)
(check-expect (editor-kh empty-editor "right") empty-editor)

(check-expect (editor-kh cursor-beginning "e") (create-editor "e" "foo"))
(check-expect (editor-kh cursor-beginning "\b") cursor-beginning)
(check-expect (editor-kh cursor-beginning "left") cursor-beginning)
(check-expect (editor-kh cursor-beginning "right") (create-editor "f" "oo"))

(check-expect (editor-kh cursor-mid "e") (create-editor "fooe" "bar"))
(check-expect (editor-kh cursor-mid "\b") (create-editor "fo" "bar"))
(check-expect (editor-kh cursor-mid "left") (create-editor "fo" "obar"))
(check-expect (editor-kh cursor-mid "right") (create-editor "foob" "ar"))

(check-expect (editor-kh cursor-end "e") (create-editor "fooe" ""))
(check-expect (editor-kh cursor-end "\b") (create-editor "fo" ""))
(check-expect (editor-kh cursor-end "left") (create-editor "fo" "o"))
(check-expect (editor-kh cursor-end "right") cursor-end)

(check-expect (editor-kh (create-editor "cd" "fgh") "e") (create-editor "cde" "fgh"))
(define (editor-kh ed ke)
  (cond [(key=? ke "left") (editor-lft ed)]
        [(key=? ke "right") (editor-rgt ed)]
        [(key=? ke "\b") (editor-del ed)]
        [(key=? ke "\t") ed]
        [(key=? ke "\r") ed]
        [(= (string-length ke) 1) (editor-ins ed ke)]
        [else ed]))

; editor-lft: Editor -> Editor
; moves the cursor of `ed` to the left
(check-expect (editor-lft empty-editor) empty-editor)
(check-expect (editor-lft cursor-beginning) cursor-beginning)
(check-expect (editor-lft cursor-mid) (create-editor "fo" "obar"))
(check-expect (editor-lft cursor-end) (create-editor "fo" "o"))
(define (editor-lft ed)
  (cond [(empty? (editor-pre ed)) ed]
        [(cons? (editor-pre ed))
         (make-editor (rest (editor-pre ed))
                      (cons (first (editor-pre ed))
                            (editor-post ed)))]))

; editor-rgt: Editor -> Editor
; moves the cursor of `ed` to the right
(check-expect (editor-rgt empty-editor) empty-editor)
(check-expect (editor-rgt cursor-beginning) (create-editor "f" "oo"))
(check-expect (editor-rgt cursor-mid) (create-editor "foob" "ar"))
(check-expect (editor-rgt cursor-end) cursor-end)
(define (editor-rgt ed)
  (cond [(empty? (editor-post ed)) ed]
        [(cons? (editor-post ed))
         (make-editor (cons (first (editor-post ed))
                            (editor-pre ed))
                      (rest (editor-post ed)))]))

; editor-del: Editor -> Editor
; deletes a character from `ed`, if any
(check-expect (editor-del empty-editor) empty-editor)
(check-expect (editor-del cursor-beginning) cursor-beginning)
(check-expect (editor-del cursor-mid)
              (make-editor (cons "o" (cons "f" '())) (cons "b" (cons "a" (cons "r" '())))))
(check-expect (editor-del cursor-end)
              (make-editor (cons "o" (cons "f" '())) '()))
(define (editor-del ed)
  (cond [(empty? (editor-pre ed)) ed]
        [(cons? (editor-pre ed)) (make-editor (rest (editor-pre ed))
                                              (editor-post ed))]))

; editor-ins: Editor KeyEvent -> Editor
; inserts the 1String `ke` into `ed`, between pre and post
(check-expect (editor-ins empty-editor "e") (make-editor (cons "e" '()) '()))
(check-expect
 (editor-ins (make-editor (cons "d" '()) (cons "f" (cons "g" '()))) "e")
 (make-editor (cons "e" (cons "d" '())) (cons "f" (cons "g" '()))))
(define (editor-ins ed ke)
  (make-editor (cons ke (editor-pre ed))
               (editor-post ed)))
