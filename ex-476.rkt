#lang htdp/isl+

(require 2htdp/abstraction)

;;; Chapter 12.8 poses a problem concerning finite state machines and strings but
;;; immediately defers to this chapter because the solution calls for generative recursion.
;;; You have now acquited the design knowledge to tackle the problem.
;;;
;;; Design the function `fsm-match`. It consumes the data representation of a finite
;;; state machine and a string. It produces #true if the sequence of characters in
;;; the string causes the finite state machine to transition from an initial state to
;;; a final state.
;;;
;;; Since this problem is about the design of generative recursive functions, we provide
;;; the essential data definition and a data example.
;;;
;;; HINT: Design the necessary auxiliary function locally to the `fsm-match?` function.
;;; In this context, represent the problem as a pair of parameters: the current state of
;;; the finite state machine and the remaining list of 1Strings.

(define-struct fsm [initial transitions final])
; An FSM is a structure:
;  (make-fsm FSM-State [List-of 1Transition] FSM-State)

; end-state?: FSM -> Boolean
; True if `an-fsm` is currently in the end state.
(define (end-state? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

; fsm-next: FSM String -> [Maybe FSM]
; Produces a new FSM with `initial` set to the state we'd wind up in after with the
; transition key `k`. Produces #false if it's not possible to transition to a new state.
(define (fsm-next an-fsm k)
  (local [(define initial (fsm-initial an-fsm))
          (define final (fsm-final an-fsm))
          (define transitions (fsm-transitions an-fsm))

          (define (next-state k)
            (local [(define next
                      (find (λ (t) (transition-matches? t initial k)) transitions))]
              (if (transition? next) (transition-next next) #false)))
          (define next (next-state k))]
    (if (string? next)
        (make-fsm next transitions final)
        #false)))


(define-struct transition [current key next])
; A 1Transition is a structure:
;  (make-transition FSM-State 1String FSM-State)

; transition-matches?: 1Transition FSM-State 1String -> Boolean
; True if `t`'s current matches `c` and its key matches `k`
(define (transition-matches? t c k)
  (and (state=? (transition-current t) c)
       (string=? (transition-key t) k)))

; An FSM-State is String.

; state=?: FSM-State FSM-State -> Boolean
; Compares s1 and s2, returns #true iff they are the same
(define (state=? s1 s2)
  (string=? s1 s2))


(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; fsm-match: FSM String -> Boolean
; does `an-fsm` recognize the given string?
(check-expect (fsm-match fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match fsm-a-bc*-d "da") #false)
(check-expect (fsm-match fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match fsm-a-bc*-d "d") #false)
(check-expect (fsm-match fsm-a-bc*-d "acbdd") #false)
(define (fsm-match an-fsm str)
  (local [(define (fsm-match/list an-fsm cs)
            (or (and (end-state? an-fsm) (empty? cs))
                (local [(define maybe-next (fsm-next an-fsm (first cs)))]
                  (and (fsm? maybe-next)
                       (fsm-match/list maybe-next (rest cs))))))]
    (fsm-match/list an-fsm (explode str))))

; find: [Any -> Boolean] [List-of Any] -> [Maybe Any]
; returns the first element in `l` that fulfills `pred`
(define (find pred l) (for/or [(e l)] (if (pred e) e #false)))
