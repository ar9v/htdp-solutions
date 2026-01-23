#lang htdp/isl+

;;; Abstract the data definitions for S-expr and SL so that they abstract over the kinds
;;; of Atoms that may appear.

; A [S-expr T] is one of
; -- T
; -- [SL T]

; [SL T] is a [List-of [S-expr T]]
