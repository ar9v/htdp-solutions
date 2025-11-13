#lang htdp/bsl

;; Revisit the structure type definitions of exercise 65. Make sensible guesses as to what
;; kind of values go with which fields. Then create at least one instance per structure
;; type definition

;;; make-movie: String String Number -> Movie
(define-struct movie [title producer year])
(make-movie "Porco Rosso" "Toshio Suzuki" 1992)

;;; make-person: String String String String -> Person
;;; Phone might have its own constrained type if you're feeling up to it...
(define-struct person [name hair eyes phone])
(make-person "Ricardo" "brown" "brown" "1111111111")

;;; make-pet: String Number -> Pet
(define-struct pet [name number])
(make-pet "Txiki" 1)

;;; make-CD: String String Number -> CD
(define-struct CD [artist title price])
(make-CD "Gentle Giant" "Three Friends" 99)

;;; make-sweater: String String String -> Sweater
(define-struct sweater [material size producer])
(make-sweater "Wool" "XXL" "Grandma")
