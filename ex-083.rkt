#lang htdp/bsl

(require 2htdp/image)

;;; Design the function `render`, which consumes an Editor and produces an image.
;;; The purpose of the function is to render the text within an empty scene of 200x20
;;; pixels. For the cursor, use a 1x20 red rectangle and for the strings, black text
;;; of size 16.

(define-struct editor [pre post])
; An Editor is a structure:
;  (make-editor String String)
;
; interpretation: (make-editor s t) describes an editor whose
; visible text is (string-append s t) with the cursor displayed
; between s and t

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
