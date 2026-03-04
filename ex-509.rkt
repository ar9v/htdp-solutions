#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Design the function `split`. Use the accumulator design recipe to improve on the
;;; result of Exercise 508. After all, the hints already point out that when the function
;;; discovers the correct split, it needs both parts of the list, and one part is obviously
;;; lost to recursion.

(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor [pre post])

; create-editor: String String -> Editor
; Create an Editor from `pre` and `post`
(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))

; main: String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
            [to-draw editor-render]
            [on-key editor-kh]
            [on-mouse editor-mouse]))

; editor-render: Editor -> Image
; renders an editor as an image of the two texts separated by the cursor
(define (editor-render e)
  (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))

; editor-text: [List-of 1String] -> Image
; renders a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

; editor-kh: Editor KeyEvent -> Editor
; deals with a key event, given some editor
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
(define (editor-lft ed)
  (cond [(empty? (editor-pre ed)) ed]
        [(cons? (editor-pre ed))
         (make-editor (rest (editor-pre ed))
                      (cons (first (editor-pre ed))
                            (editor-post ed)))]))

; editor-rgt: Editor -> Editor
; moves the cursor of `ed` to the right
(define (editor-rgt ed)
  (cond [(empty? (editor-post ed)) ed]
        [(cons? (editor-post ed))
         (make-editor (cons (first (editor-post ed))
                            (editor-pre ed))
                      (rest (editor-post ed)))]))

; editor-del: Editor -> Editor
; deletes a character from `ed`, if any
(define (editor-del ed)
  (cond [(empty? (editor-pre ed)) ed]
        [(cons? (editor-pre ed)) (make-editor (rest (editor-pre ed))
                                              (editor-post ed))]))

; editor-ins: Editor KeyEvent -> Editor
; inserts the 1String `ke` into `ed`, between pre and post
(define (editor-ins ed ke)
  (make-editor (cons ke (editor-pre ed))
               (editor-post ed)))


; editor-parts?: String -> [Editor -> Boolean]
; Given `ed-string`, returns a predicate that determines whether a given Editor's `pre` and
; `post` form `ed-string`
(check-expect ((editor-parts? "foobar") (make-editor (explode "of") (explode "obar")))
              #true)
(define (editor-parts? ed-string)
  (λ (ed)
    (string=? (implode (append (reverse (editor-pre ed)) (editor-post ed)))
              ed-string)))

; editor-mouse: Editor x y MouseEvent -> Editor
(define (editor-mouse ed x _y me)
  (local [(define p (reverse (editor-pre ed)))
          (define s (editor-post ed))
          (define as-lo1s (append p s))]
    (if (mouse=? me "button-down")
        (split as-lo1s x)
        ed)))

; splits-editor-text-image?: N -> [Editor -> Boolean]
; Given a natural number `x`, returns a predicate that determines whether a given Editor
; is split by `x` as explained by condition (2) in the problem statement
(define (splits-editor-text-image? x)
  (λ (ed) (is-split-by? ed x)))

; is-split-by?: Editor N -> Boolean
; True if the given Editor `ed`'s text is split by a cursor at position `x`
(define (is-split-by? ed x)
  (local [(define p (reverse (editor-pre ed)))
          (define s (editor-post ed))]
    (and (<= (image-width (editor-text p)) x)
         (or (empty? s)
             (<= x (image-width (editor-text (append p (list (first s))))))))))

; split: [List-of 1String] N -> Editor
; Returns an editor whose `pre` and `post` are made up of `ed` if its `editor-text`
; image were split by a cursor at coordinate `x`
(check-satisfied (split '() 0) (editor-parts? ""))
(check-satisfied (split '() 0) (splits-editor-text-image? 0))
(check-satisfied (split '() 15) (editor-parts? ""))
(check-satisfied (split '() 15) (splits-editor-text-image? 15))
(check-satisfied (split (explode "hello") 0) (editor-parts? "hello"))
(check-satisfied (split (explode "hello") 0) (splits-editor-text-image? 0))
(check-satisfied (split (explode "hello, world!") 15)
                 (editor-parts? "hello, world!"))
(check-satisfied (split (explode "hello, world!") 15)
                 (splits-editor-text-image? 15))
(define (split ed x)
  (local [; split: [List-of 1String] [List-of 1String] -> Editor
          ;
          ; accumulator pre: Represents the current prefix, or, in other words, the
          ; letters in `ed` that are not in `post`
          (define (split/a post pre)
            (cond [(empty? post) (make-editor pre '())]
                  [else (local [(define candidate (make-editor pre post))]
                          (if (is-split-by? candidate x)
                              candidate
                              (split/a (rest post) (cons (first post) pre))))]))]
    (split/a ed '())))
