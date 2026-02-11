#lang htdp/isl+

;;; When you add two inexact numbers of vastly different orders of magnitude, you may get
;;; the larger one back as the result. For example, if a number system uses only 15
;;; significant digits, we run into problems when adding numbers that vary by more than
;;; a f actor of 10^16:
;;;
;;; 1.0 * 10^16 + 1 = 1.00000000000000001 * 10^16
;;;
;;; but the closest representable answer is 10^16.
;;;
;;; At first glance, this approximation doesn't look too bad. Being wrong by one part in
;;; 10^16 (ten million billion) is close enough to the truth. Unfortunately, this kind of
;;; problem can add up to huge problems. Consider the list of numbers in figure 144 and
;;; determine the values of these expressions:
;;;
;;; -- (sum JANUS)
;;; -- (sum (reverse JANUS))
;;; -- (sum (sort JANUS <))

(define (sum l) (foldl + 0 l))

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

(sum JANUS)                             ; #i99.0
(sum (reverse JANUS))                   ; #i-12345678901235e+80
(sum (sort JANUS <))                    ; #i0.0

;;; Assuming `sum` adds the numbers in a list from left to right, explain what these
;;; expressions compute. What do you think of the results?
;;;
;;; A: All expressions compute the sum of the numbers in JANUS, but the differing
;;; arrangements make it so that we get different results.
;;;
;;; Sorting the list causes the running sum to be #i-3.2e+270 for most of the
;;; computation, up until the last sum, producing #i0.0. In the other arrangements, we
;;; wind up with a running sum that sways back and forth between small and large numbers,
;;; up until near the end, where they cancel out, leaving the very last sum to determine
;;; the final value (e.g. `(sum JANUS)` will have a running sum of #i8e+269 up until it
;;; gets cancelled out by #i-8e+269, leaving 0.0 and 99.0 to determine the final result,
;;; which is 99.0)

;;; Generic advice on inexact calculations tells programmers to start additions with the
;;; smallest numbers. While adding a big number to two small numbers might yield the
;;; big one, adding small numbers first creates a large one, which might change the
;;; outcome:
;;;
;;; > (expt 2 #i53.0)
;;; #i9007199254740992.0
;;; > (sum (list #i1.0 (expt 2 #i53.0)))
;;; #i9007199254740992.0
;;; > (sum (list #i1.0 #i1.0 (expt 2 #i53.0)))
;;; #i9007199254740994.0
;;;
;;; This trick may *not* work; se the JANUS interactions above.

;;; In a language such as ISL+, you can convert the numbers to exact rationals, use
;;; exact arithmetic on those, and convert the result back:
;;;
;;; (exact->inexact (sum (map inexact->exact JANUS)))
;;;
;;; Evaluate this expression and compare the result to the three sums above. What do you
;;; think now about advice from the web?

(exact->inexact (sum (map inexact->exact JANUS))) ; #i4.2344294738446e+170

;;; Considering that `(sum (sort JANUS <))` is way off compared to the actual answer, it's
;;; safe to say that the advice is not foolproof.
