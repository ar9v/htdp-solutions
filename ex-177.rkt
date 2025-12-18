#lang htdp/bsl

;;; Design the function `create-editor`. The function consumes two strings and produces
;;; an Editor. The first string is the text to the left of the cursor and the second
;;; string is the text to the right of the cursor.

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
