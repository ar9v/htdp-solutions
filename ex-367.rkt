#lang htdp/isl+

;;; The design recipe calls for a self-reference in the template for `xexpr-attr`. Add
;;; this self-reference to the template and then explain why the finished parsing function
;;; does not contain it.

; (define (xexpr-attr xe)
;   (local ((define optional-loa+content (rest xe)))
;     (cond
;       [(empty? optional-loa+content) ...]
;       [else (... (first optional-loa+content)
;              ... (x-expr-attr (rest optional-loa+content)) ...)])))

;;; A:
;;; The final version doesn't need it because it's only concerned with the list of
;;; attributes, i.e. in terms of Xexpr, the function only cares about the second element.
;;;
;;; A way of seeing it is that Xexpr is a list, but a heterogeneous one.
