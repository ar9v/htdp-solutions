#lang htdp/bsl+

;;; Design a program that sorts lists of game players by score:

(define-struct gp [name score])
; A GamePlayer is a structure:
;   (make-gp String Number)
;
; interpretation: (make-gp p s) represents player p who scored a maximum of s points

(define gp1 (make-gp "Player1" 10))
(define gp2 (make-gp "Player2" 20))
(define gp3 (make-gp "Player3" 30))

;;; HINT: Formulate a function that compares two elements of GamePlayer

; sort>-gps: List<GamePlayer> -> List<GamePlayer>
; Rearranges `logp` in descending order, according to their score
(check-satisfied (sort>-gps '()) sorted>-gps?)
(check-satisfied (sort>-gps (list gp1 gp2 gp3)) sorted>-gps?)
(check-satisfied (sort>-gps (list gp3 gp2 gp1)) sorted>-gps?)
(check-satisfied (sort>-gps (list gp2 gp1 gp3)) sorted>-gps?)
(define (sort>-gps logp)
  (cond [(empty? logp) '()]
        [(cons? logp)
         (insert (first logp) (sort>-gps (rest logp)))]))

; sorted>-gps?: List<GamePlayer> -> Boolean
; Determines whether `logp` is sorted in descending order
(check-expect (sorted>-gps? '()) #true)
(check-expect (sorted>-gps? (list gp1 gp2)) #false)
(check-expect (sorted>-gps? (list gp3 gp2 gp1)) #true)
(define (sorted>-gps? logp)
  (cond [(or (empty? logp) (empty? (rest logp))) #true]
        [else
         (and (gp> (first logp) (first (rest logp)))
              (sorted>-gps? (rest logp)))]))

; insert: GamePlayer List<GamePlayer> -> List<GamePlayer>
; Inserts `gp` in a sorted descending list of players, `logp`, keeping its order.
(check-expect (insert gp1 '()) (list gp1))
(check-expect (insert gp2 (list gp1)) (list gp2 gp1))
(check-expect (insert gp1 (list gp2)) (list gp2 gp1))
(check-expect (insert gp2 (list gp3 gp1)) (list gp3 gp2 gp1))
(define (insert gp logp)
  (cond [(empty? logp) (list gp)]
        [(cons? logp)
         (if (gp< gp (first logp))
             (cons (first logp) (insert gp (rest logp)))
             (cons gp logp))]))

; gp>: GamePlayer GamePlayer -> Boolean
; Determines if gp1 is greater than gp2
; interpretation: does gp1 have a greater score?
(define (gp> gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))

; gp<: GamePlayer GamePlayer -> Boolean
; Determines if gp1 is lower than gp2
; interpretation: does gp1 have a lower score?
(define (gp< gp1 gp2)
  (< (gp-score gp1) (gp-score gp2)))
