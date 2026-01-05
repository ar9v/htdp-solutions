#lang htdp/bsl+

(require 2htdp/image)

;;; When you are presented with a complex data definition -- like the one for the state
;;; of a Tetris game -- you start by creating instances of the various data collections.
;;; Here are some suggestive names for examples you can later use for functional examples:
;;;
;;; (define landscape0 ...)
;;; (define block-dropping ...)
;;; (define tetris0 ...)
;;; (define tetris0-drop ...)
;;; ...
;;; (define block-landed (make-block 0 (- HEIGHT 1)))
;;; ...
;;; (define block-on-block (make-block 0 (- HEIGHT 2)))

;;; Design the program `tetris-render`, which turns a given instance of Tetris into an
;;; Image. Use DrRacket's interactions area to develop the expression that renders some
;;; of your (extremely) simple data examples. Then formulate the functional examples as
;;; unit tests and the function itself.

(define WIDTH 10) ; # of blocks, horizontally
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))

(define BLOCK
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))

(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)
;
; A Landscape is one of:
; -- '()
; -- (cons Block Landscape)
;
; A Block is a structure:
;   (make-block N N)
;
; interpretations:
; (make-block x y) depicts a block whose left corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top. x and y are, therefore, in the range [0, SIZE)
;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the dropping block, while b1, b2,
; and ... are resting.

(define block-dropping (make-block 2 3))
(define dropped-block1 (make-block 0 (- SIZE 1)))
(define dropped-block2 (make-block (- SIZE 1) (- SIZE 1)))
(define empty-landscape '())
(define landscape0 (list dropped-block1 dropped-block2))

(define tetris0 (make-tetris block-dropping empty-landscape))
(define tetris0-drop (make-tetris block-dropping landscape0))

; tetris-render: Tetris -> Image
; Renders the given `tetris` state on BACKGROUND
(check-expect
 (tetris-render tetris0)
 (block-render (tetris-block tetris0)
               (landscape-render (tetris-landscape tetris0)
                                 BACKGROUND)))
(check-expect
 (tetris-render tetris0-drop)
 (block-render (tetris-block tetris0-drop)
               (landscape-render (tetris-landscape tetris0-drop)
                                 BACKGROUND)))
(define (tetris-render tetris)
  (block-render (tetris-block tetris)
                (landscape-render (tetris-landscape tetris)
                                  BACKGROUND)))

; block-render: Block Image -> Image
; Places BLOCK in `img`
(check-expect
 (block-render block-dropping BACKGROUND)
 (place-image/align BLOCK
                    (* (block-x block-dropping) SIZE) (* (block-y block-dropping) SIZE)
                    "left" "top"
                    BACKGROUND))
(define (block-render block img)
  (place-image/align BLOCK
                     (* (block-x block) SIZE) (* (block-y block) SIZE)
                     "left" "top"
                     img))

; landscape-render: Landscape Image -> Image
; Places the blocks in `landscape` on `img`
(check-expect
 (landscape-render empty-landscape BACKGROUND) BACKGROUND)
(check-expect
 (landscape-render landscape0 BACKGROUND)
 (block-render dropped-block1
               (block-render dropped-block2
                             BACKGROUND)))
(define (landscape-render landscape img)
  (cond [(empty? landscape) img]
        [(cons? landscape)
         (block-render (first landscape)
                       (landscape-render (rest landscape) img))]))
