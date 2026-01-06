#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Design the interactive program `tetris-main`, which displays blocks dropping in a
;;; straight line from the top of the canvas and landing on the floor or on blocks that
;;; are already resting. The input to `tetris-main` should determine the rate at which
;;; the clock ticks. See the documentation of `on-tick` for how to specify the rate.
;;;
;;; When a block lands, your program should immediately create another block that descends
;;; on the column to the right of the current one. If the current block is already in the
;;; right-most column, the next block should use the left-most one. Alternatively, define
;;; the function `block-generate`, which randomly selects a column different from the
;;; current one; see exercise 219 for inspiration.

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

(define block-dropping (make-block 0 0))
(define block-above-dropped-block1 (make-block 0 (- SIZE 2)))
(define block-about-to-rest (make-block 1 (- SIZE 1)))
(define dropped-block1 (make-block 0 (- SIZE 1)))
(define dropped-block2 (make-block (- SIZE 1) (- SIZE 1)))

(define empty-landscape '())
(define landscape0 (list dropped-block1 dropped-block2))

(define tetris0 (make-tetris block-dropping empty-landscape))
(define tetris0-drop (make-tetris block-dropping landscape0))
(define tetris0-about-to-rest
  (make-tetris block-above-dropped-block1 landscape0))
(define tetris0-about-to-rest-on-ground
  (make-tetris block-about-to-rest landscape0))

; tetris-main: Number -> Tetris
; Runs the Tetris game. Takes in the rate at which the game runs.
(define (tetris-main rate)
  (big-bang tetris0
            [to-draw tetris-render]
            [on-tick tetris-update rate]))

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

; tetris-update: Tetris -> Tetris
; Updates the `tetris` state by dropping its block
(check-expect
 (tetris-update tetris0)
 (make-tetris (block-update (tetris-block tetris0)) (tetris-landscape tetris0)))
(check-random
 (tetris-update tetris0-about-to-rest)
 (make-tetris (block-generate block-above-dropped-block1)
              (cons block-above-dropped-block1 landscape0)))
(check-random
 (tetris-update tetris0-about-to-rest-on-ground)
 (make-tetris (block-generate block-about-to-rest)
              (cons block-about-to-rest landscape0)))
(define (tetris-update t)
  (if (tetris-block-resting? t)
      (make-tetris (block-generate (tetris-block t))
                   (cons (tetris-block t) (tetris-landscape t)))
      (make-tetris (block-update (tetris-block t))
                   (tetris-landscape t))))

; block-update: Block -> Block
; Increments block `b`'s y-coordinate.
(check-expect (block-update block-dropping)
              (block-inc-y block-dropping))
(check-expect (block-update dropped-block1)
              (block-inc-y dropped-block1))
(define (block-update b)
  (block-inc-y b))

; block-generate: Block -> Block
; Generates a new block to be dropped. Could use the linear or random heuristics.
(check-random (block-generate dropped-block1)
              (block-generate-random dropped-block1))
(check-random (block-generate dropped-block2)
              (block-generate-random dropped-block2))
(define (block-generate b)
  (block-generate-random b))

; block-generate-linear: Block -> Block
; Generates a new block to be dropped. Starts from the top, one column to the right of the
; column where the block was dropped.
(check-expect (block-generate-linear dropped-block1)
              (make-block (add1 (block-x dropped-block1)) 0))
(check-expect (block-generate-linear dropped-block2)
              (make-block (remainder (add1 (block-x dropped-block2))
                                     SIZE)
                          0))
(define (block-generate-linear b)
  (make-block (remainder (add1 (block-x b)) SIZE) 0))

; block-generate-random: Block -> Block
; Generates a new block to be dropped. Randomly picks an x coordinate, as long as its not
; the x coordinate of the last-dropped block.
(check-satisfied (block-generate-random dropped-block1) not-x-0?)
(define (block-generate-random b)
  (block-generate-check-random b (make-block (random SIZE) 0)))

; not-x-0?: Block -> Boolean
; Predicate for testing. True if `b`'s x-coordinate is not 0
(define (not-x-0? b)
  (not (zero? (block-x b))))

; block-generate-check-random: Block Block -> Block
; Checks if `candidate` shares an x-coordinate with `current`. If it does, tries another
; candidate until it gets a random block with a different x-coordinate.
(check-expect (block-generate-check-random dropped-block1 (make-block 2 0))
              (make-block 2 0))
(define (block-generate-check-random current candidate)
  (if (equal? (block-x current) (block-x candidate))
      (block-generate-random current)
      candidate))

; tetris-block-resting?: Tetris -> Boolean
; True if a Tetris' block is on top of a resting block or the ground
(check-expect (tetris-block-resting? tetris0) #false)
(check-expect (tetris-block-resting? tetris0-drop) #false)
(check-expect (tetris-block-resting? tetris0-about-to-rest) #true)
(check-expect (tetris-block-resting? tetris0-about-to-rest-on-ground) #true)
(define (tetris-block-resting? t)
  (or (member? (block-inc-y (tetris-block t))
               (tetris-landscape t))
      (block-grounded? (tetris-block t))))

; block-grounded?: Block -> Boolean
; True if block's y (logical) coordinate is at SIZE - 1
(check-expect (block-grounded? block-dropping) #false)
(check-expect (block-grounded? block-above-dropped-block1) #false)
(check-expect (block-grounded? dropped-block1) #true)
(check-expect (block-grounded? dropped-block2) #true)
(define (block-grounded? b)
  (= (add1 (block-y b)) SIZE))

; block-inc-y: Block -> Block
; Returns a new block, with an incremented y value (or SIZE - 1)
(check-expect (block-update block-dropping)
              (make-block (block-x block-dropping) (add1 (block-y block-dropping))))
(check-expect (block-update dropped-block1)
              dropped-block1)
(define (block-inc-y b)
  (if (block-grounded? b)
      b
      (make-block (block-x b) (add1 (block-y b)))))
