#lang htdp/isl+

;;; Interpret the following elements of Xexpr.v2 as XML data.
;;;
;;; Which ones are elements of Xexpr.v0 or Xexpr.v1?

'(server ((name "example.org")))
; <server name="example.org" />
;
; Xexpr.v2

'(carcas (board (grass)) (player ((name "sam"))))
; <carcas>
;   <board>
;     <grass>
;   </board>
;
;   <player name="sam" />
; </carcas>
;
; Xexpr.v2


'(start)
; <start />
;
; Xexpr.v0
