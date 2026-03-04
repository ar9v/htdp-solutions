#lang htdp/isl+

(require 2htdp/image)

;;; Design `split-structural` using the structural design recipe. The function consumes
;;; a list of 1Strings `ed` and a natural number `x`; the former represents the complete
;;; string in some Editor and the latter the x-coordinate of the mouse click. The
;;; function produces
;;;
;;; (make-editor p s)
;;;
;;; such that (1) `p` and `s` make up `ed` and (2) `x` is larger than the image of `p` and
;;; smaller than the image of `p` extended with the first 1String on `s` (if any).
;;;
;;; Here is the first condition expressed with an ISL+ expression:
;;;
;;; (string=? (string-append p s) ed)
;;;
;;; The second one is:
;;;
;;; (<= (image-width (editor-text p))
;;;     x
;;;     (image-width (editor-text (append p (first s)))))
;;;
;;; assuming `(cons? s)`

(define FONT-SIZE 11)
(define FONT-COLOR "black")

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
;
; interpretation: if (make-editor p s) is the state of an interactive editor,
; (reverse p) corresponds to the text to the left of the cursor and s to the text on
; the right

; [List-of 1String] -> Image
; renders a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

; editor-parts?: String -> [Editor -> Boolean]
; Given `ed-string`, returns a predicate that determines whether a given Editor's `pre` and
; `post` form `ed-string`
(check-expect ((editor-parts? "foobar") (make-editor (explode "of") (explode "obar")))
              #true)
(define (editor-parts? ed-string)
  (λ (ed)
    (string=? (implode (append (reverse (editor-pre ed)) (editor-post ed)))
              ed-string)))

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

; split-structural: [List-of 1String] N -> Editor
; Returns an editor whose `pre` and `post` are made up of `ed` if its `editor-text`
; image were split by a cursor at coordinate `x`
(check-satisfied (split-structural '() 0) (editor-parts? ""))
(check-satisfied (split-structural '() 0) (splits-editor-text-image? 0))
(check-satisfied (split-structural '() 15) (editor-parts? ""))
(check-satisfied (split-structural '() 15) (splits-editor-text-image? 15))
(check-satisfied (split-structural (explode "hello") 0) (editor-parts? "hello"))
(check-satisfied (split-structural (explode "hello") 0) (splits-editor-text-image? 0))
(check-satisfied (split-structural (explode "hello, world!") 15)
                 (editor-parts? "hello, world!"))
(check-satisfied (split-structural (explode "hello, world!") 15)
                 (splits-editor-text-image? 15))
(define (split-structural ed x)
  (local [(define (find-prefix ps)
            (cond [(empty? ps) (make-editor '() '())]
                  [else (local [(define f (first ps))]
                          (if (is-split-by? f x)
                              f
                              (find-prefix (rest ps))))]))

          (define (editor-prefixes ed)
            (cond [(empty? ed) '()]
                  [else (local [(define pre (list (first ed)))
                                (define post (rest ed))]
                          (append (list (make-editor pre post))
                                  (map (λ (e) (make-editor (append (editor-pre e) pre)
                                                           (editor-post e)))
                                       (editor-prefixes post))))]))]
    (cond [(empty? ed) (make-editor '() '())]
          [(< x (image-width (editor-text (list (first ed))))) (make-editor '() ed)]
          [else (find-prefix (editor-prefixes ed))])))
