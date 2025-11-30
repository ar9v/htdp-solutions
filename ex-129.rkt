#lang htdp/bsl

;;; Create BSL lists that represent
;;;
;;; 1. a list of celestial bodies, say, at least all the planets in our solar system.
;;; 2. a list of items for a meal, for example, steak, french fries, beans, bread, water
;;;    Brie cheese, and ice cream.
;;; 3. a list of colors

;;; 1
(define planets
  (cons "Mercury"
        (cons "Venus"
              (cons "Earth"
                    (cons "Mars"
                          (cons "Jupiter"
                                (cons "Saturn"
                                      (cons "Uranus"
                                            (cons "Neptune"
                                                  '())))))))))

;;; 2
(define meal
  (cons "steak"
        (cons "french fries"
              (cons "beans"
                    (cons "bread"
                          (cons "water"
                                (cons "Brie cheese"
                                      (cons "ice cream"
                                            '()))))))))
;;; 3
(define colors
  (cons "black"
        (cons "white"
              (cons "red"
                    (cons "blue"
                          (cons "yellow"
                                '()))))))

;;; I'll skip the sketches; too elaborate for my ASCII-art proficiency. (I prefer
;;; box-and-arrow diagrams)
