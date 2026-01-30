#lang htdp/isl+

;;; Represent this XML data as elements of Xexpr.v2

; <transition from="seen-e" to="seen-f" />
'(transition ((from "seen-e") (to "seen-f")))

; <ul><li><word /><word /></li><li><word /></li></ul>
'(ul
  (li (word) (word))
  (li (word)))

;;; Which one could be represented in Xexpr.v0 or Xexpr.v1?
;;;
;;; A: The first one can only be expressed in terms of Xexpr.v2, but the second one could
;;;    also be expressed in terms of Xexpr.v1
