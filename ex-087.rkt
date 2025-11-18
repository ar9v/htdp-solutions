#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Develop a data representation for an editor based on our first idea, using a string
;;; and an index. Then solve the preceding exercises again.
;;;
;;; Retrace the design recipe. Hint: if you haven't done so, solve the exercises in
;;; chapter 2.1

(define-struct editor [text cursor])
; An Editor is a structure:
;   (make-editor String PositiveInteger)
;
; interpretation: (make-editor t i)
; Represents an editor whose whole text is `t`, and which has a cursor at position `i`,
; counting from the left.

(define empty-ed (make-editor "" 0))
(define cursor-at-beg (make-editor "hello" 0))
(define cursor-in-mid (make-editor "hello world" 5))
(define cursor-at-end (make-editor "hello" 5))

; Constants
(define FONT-SIZE 16)
(define CURSOR-WIDTH 1)
(define CURSOR-HEIGHT (+ FONT-SIZE 4))
(define EDITOR-WIDTH 200)
(define EDITOR-HEIGHT CURSOR-HEIGHT)

; Graphical Constants
(define FONT-COLOR "black")
(define CURSOR-COLOR "red")
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" CURSOR-COLOR))
(define EDITOR (empty-scene EDITOR-WIDTH EDITOR-HEIGHT))

; run: Editor -> Editor
(define (run text)
  (big-bang (make-editor text (string-length text))
            [to-draw render]
            [on-key edit]))

; render: Editor -> Image
; Given Editor `e`, render a text image `e`'s text string, along CURSOR, in EDITOR
(check-expect (render empty-ed)
              (overlay/left (beside (render-text "") CURSOR) EDITOR))
(check-expect (render cursor-at-beg)
              (overlay/left (beside CURSOR (render-text "hello")) EDITOR))
(check-expect (render cursor-in-mid)
              (overlay/left
               (beside (render-text (editor-text-pre cursor-in-mid))
                       CURSOR
                       (render-text (editor-text-post cursor-in-mid)))
               EDITOR))
(check-expect (render cursor-at-end)
              (overlay/left (beside (render-text "hello") CURSOR) EDITOR))
(define (render ed)
  (overlay/left
   (beside (render-text (editor-text-pre ed)) CURSOR (render-text (editor-text-post ed)))
   EDITOR))

; overlay/left: Image Image -> Image
; Overlays i1 on top of i2 aligned to the left (x-axis) and center (y-axis)
(check-expect (overlay/left CURSOR EDITOR) (overlay/align "left" "center" CURSOR EDITOR))
(define (overlay/left i1 i2) (overlay/align "left" "center" i1 i2))

; render-text: String -> Image
; Returns an text image representation of `s`, with editor-specific formatting
(check-expect (render-text "hello") (text "hello" FONT-SIZE FONT-COLOR))
(define (render-text s) (text s FONT-SIZE FONT-COLOR))

; editor-text-pre: Editor -> String
; Given an Editor `e`, return the text that comes before editor-cursor
(check-expect (editor-text-pre empty-ed) "")
(check-expect (editor-text-pre cursor-at-beg) "")
(check-expect (editor-text-pre cursor-in-mid) "hello")
(check-expect (editor-text-pre cursor-in-mid) "hello")
(define (editor-text-pre ed)
  (substring (editor-text ed) 0 (editor-cursor ed)))

; editor-text-post: Editor -> String
; Given an Editor `e`, return the text that comes after editor-cursor
(check-expect (editor-text-post empty-ed) "")
(check-expect (editor-text-post cursor-at-beg) "hello")
(check-expect (editor-text-post cursor-in-mid) " world")
(check-expect (editor-text-post cursor-at-end) "")
(define (editor-text-post ed)
  (substring (editor-text ed) (editor-cursor ed) (string-length (editor-text ed))))

; edit: Editor KeyEvent -> Editor
; Given an Editor `e` and a KeyEvent `ke`, returns an Editor with a new string for
; editor-text and a new editor-cursor depending on the event.
;
; If the key event is \b, editor-text is updated to remove the 1String at position
; editor-cursor
;
; If the key event is left or right, the editor-cursor is updated, if possible.
;
; If the key event is any non-escaped 1String, editor-text is updated to insert the
; 1String at cursor position editor-cursor
;
; All other events are ignored
(check-expect (edit empty-ed "\b") empty-ed)
(check-expect (edit empty-ed "left") empty-ed)
(check-expect (edit empty-ed "right") empty-ed)
(check-expect (edit empty-ed "\r") empty-ed)
(check-expect (edit empty-ed "control") empty-ed)
(check-expect (edit empty-ed "a") (make-editor "a" 1))

(check-expect (edit cursor-at-beg "\b") cursor-at-beg)
(check-expect (edit cursor-at-beg "left") cursor-at-beg)
(check-expect (edit cursor-at-beg "right") (editor-cursor-inc cursor-at-beg))
(check-expect (edit cursor-at-beg "\r") cursor-at-beg)
(check-expect (edit cursor-at-beg "control") cursor-at-beg)
(check-expect (edit cursor-at-beg "a") (editor-text-insert cursor-at-beg "a"))

(check-expect (edit cursor-in-mid "\b") (editor-text-backspace cursor-in-mid))
(check-expect (edit cursor-in-mid "left") (editor-cursor-dec cursor-in-mid))
(check-expect (edit cursor-in-mid "right") (editor-cursor-inc cursor-in-mid))
(check-expect (edit cursor-in-mid "\r") cursor-in-mid)
(check-expect (edit cursor-in-mid "control") cursor-in-mid)
(check-expect (edit cursor-in-mid "a") (editor-text-insert cursor-in-mid "a"))

(check-expect (edit cursor-at-end "\b") (editor-text-backspace cursor-at-end))
(check-expect (edit cursor-at-end "left") (editor-cursor-dec cursor-at-end))
(check-expect (edit cursor-at-end "right") (editor-cursor-inc cursor-at-end))
(check-expect (edit cursor-at-end "\r") cursor-at-end)
(check-expect (edit cursor-at-end "control") cursor-at-end)
(check-expect (edit cursor-at-end "a") (editor-text-insert cursor-at-end "a"))
(define (edit ed ke)
  (cond [(key=? ke "left") (editor-cursor-dec ed)]
        [(key=? ke "right") (editor-cursor-inc ed)]
        [(key=? ke "\b") (editor-text-backspace ed)]
        [(and (1string? ke)
              (not (escaped? ke))
              (not (overflows? (editor-text-insert ed ke))))
         (editor-text-insert ed ke)]
        [else ed]))

; editor-cursor-dec: Editor -> Editor
; Return a new Editor where the cursor for Editor e is decremented
;
; If the cursor is 0, returns 0
(check-expect (editor-cursor-dec cursor-at-beg) cursor-at-beg)
(check-expect (editor-cursor-dec cursor-in-mid)
              (make-editor (editor-text cursor-in-mid) 4))
(define (editor-cursor-dec ed)
  (make-editor (editor-text ed) (max 0 (sub1 (editor-cursor ed)))))

; editor-cursor-inc: Editor -> Editor
; Return a new Editor where the cursor for Editor e is incremented
;
; If the cursor is (string-length e), returns (string-length e)
(check-expect (editor-cursor-inc cursor-at-beg)
              (make-editor (editor-text cursor-at-beg) 1))
(check-expect (editor-cursor-inc cursor-at-end) cursor-at-end)
(define (editor-cursor-inc ed)
  (make-editor (editor-text ed) (min (string-length (editor-text ed))
                                     (add1 (editor-cursor ed)))))

; editor-text-backspace: Editor -> Editor
; Given an Editor `e`, Return a new Editor where `(editor-text e)` is a new string where
; the 1String before the cursor has been deleted.
(check-expect (editor-text-backspace empty-ed) empty-ed)
(check-expect (editor-text-backspace cursor-in-mid) (make-editor "hell world" 4))
(check-expect (editor-text-backspace cursor-at-end) (make-editor "hell" 4))
(define (editor-text-backspace ed)
  (if (= (editor-cursor ed) 0)
      ed
      (editor-cursor-dec
       (make-editor (string-delete (editor-text ed) (sub1 (editor-cursor ed)))
                    (editor-cursor ed)))))

; editor-text-insert: Editor String -> Editor
; Inserts `s` into `e` at position `(editor-cursor e)`
(define (editor-text-insert ed s)
  (make-editor (string-insert s (editor-text ed) (editor-cursor ed))
               (add1 (editor-cursor ed))))

; string-insert: String String PositiveInteger
; Creates a new string resulting from inserting `s` in `into` at position `i`
(check-expect (string-insert "_" "hello" 0) "_hello")
(check-expect (string-insert "_" "hello" 2) "he_llo")
(check-expect (string-insert "_" "hello" 5) "hello_")
(define (string-insert s into i)
  (string-append (substring into 0 i) s (substring into i)))

; string-delete: String PositiveInteger -> String
; Creates a new string resulting from deleting the 1String at the `i`th position from `s`
; If `s` is the empty string, it just returns `s`.
(check-expect (string-delete "" 0) "")
(check-expect (string-delete "" 5) "")
(check-expect (string-delete "hello" 0) "ello")
(check-expect (string-delete "hello" 4) "hell")
(define (string-delete s i)
  (if (string=? s "")
      ""
      (string-append (substring s 0 i) (substring s (add1 i)))))

; 1string?: String -> Boolean
; Given a String `s`, returns #true if it is a 1String, i.e. if it's length is 1
(define (1string? s) (= (string-length s) 1))

; escaped?: 1String -> Boolean
; Given a 1String `s`, returns #true if it is escaped. E.g. `\r`, `\b` and `\t` are all
; escaped 1Strings
(define (escaped? os)
  (or (string=? os "\r") (string=? os "\b") (string=? os "\t")))

; overflows?: Editor -> Boolean
; True if rendering Editor `e` would result in an image with a width that exceeds
; EDITOR-WIDTH
(check-expect (overflows? empty-ed) #false)
(check-expect (overflows? (make-editor (replicate EDITOR-WIDTH "h") 1)) #true)
(define (overflows? ed) (< EDITOR-WIDTH (image-width (render ed))))
