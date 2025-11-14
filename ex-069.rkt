#lang htdp/bsl

;;; Draw box representations for the solution of exercise 65

;; (define-struct movie [title producer year])
;;
;;                                    +-------+
;;                                    | movie |
;; +---------------+---------------+--+-------+
;; | Title         | Producer      | Year     |
;; | "Porco Rosso" |"Toshio Suzuki"| 1992     |
;; +---------------+---------------+----------+

;; (define-struct person [name hair eyes phone])
;;
;;                              +----------+
;;                              |Person    |
;; +----------+--------+--------+----------+
;; |Name      |Hair    |Eyes    |Phone     |
;; |"Ricardo" |"brown" |"brown" |"11111111"|
;; +----------+--------+--------+----------+


;; (define-struct pet [name number])
;;
;;         +-------+
;;         |Pet    |
;; +-------+-------+
;; |Name   |Number |
;; |"Txiki |1      |
;; --------+-------|


;; (define-struct CD [artist title price])
;;
;;                                 +-------+
;;                                 | CD    |
;; +--------------+----------------+-------+
;; |Artist        | Title          | Price |
;; |"Gentle Giant"| "Three Friends"| 99    |
;; +--------------+----------------+-------+


;; (define-struct sweater [material size producer])
;;
;;                  +----------+
;;                  | Sweater  |
;; +---------+------+----------+
;; |Material |Size  |Producer  |
;; |"Wool"   |"XXL" |"Grandma" |
;; +---------+------+----------+
