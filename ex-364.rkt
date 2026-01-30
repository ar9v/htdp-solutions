#lang htdp/isl+

;;; Represent this XML data as elements of Xexpr.v2

; <transition from="seen-e" to="seen-f" />
'(transition ((from "seen-e") (to "seen-f")))

; <ul><li><word /><word /></li><li><word /></li></ul>
'(ul
  (li (word) (word))
  (li (word)))
