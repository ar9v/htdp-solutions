#lang htdp/isl+

;;; JANUS is just a fixed list, but take a look at this function:

(define (oscillate n)
  (local [(define (O i)
            (cond [(> i n) '()]
                  [else (cons (expt #i-0.99 i) (O (+ i 1)))]))]
    (O 1)))

;;; Applying `oscillate` to a natural number `n` produces the first `n` elements of a
;;; mathematical series. It is best understood as a graph, like the one in figure 145.
;;; Run `(oscillate 15)` in DrRacket and inspect the result.

(define oscillate-15 (oscillate 15))

;;; Summing its results from left to right computes a different result than from
;;; right to left:

(define (sum l) (foldl + 0 l))

(sum (oscillate #i1000.0))
(sum (reverse (oscillate #i1000.0)))

;;; Again, the difference may appear to be small until we see the context:

(- (* 1e+16 (sum (oscillate #i1000.0)))
   (* 1e+16 (sum (reverse (oscillate #i1000.0)))))

;;; Can this difference matter? Can we trust computers?
;;;
;;; A:
;;; It can matter! It really depends on the domain of our problem: for some systems,
;;; this sort of margin is unacceptable, and could be dangerous!
;;;
;;; Therefore, we cannot trust computers! (/j)
;;;
;;; We can trust them, but we must be aware of these limitations. In some cases, we can
;;; model our data in such a way that we can work around these issues, e.g. by trying
;;; to use integers as much as possible (think representing money as whole cents, vs.
;;; as amounts, with decimals)
