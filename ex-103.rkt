#lang htdp/bsl

;;; Develop a data representation for the following four kinds of zoo animals:
;;;
;;; --- spiders: whose relevant attributes are the number of remaining legs
;;; (we assume that spiders can lose legs in accidents) and the space they
;;; need in case of transport
;;;
;;; --- elephants: whose only attributes are the space they need in case of transport
;;;
;;; --- boa constrictors: whose attributes include length and girth
;;;
;;; --- armadillos: for which you must determine appropriate attributes, including one
;;; that determines the space needed for transport

(define-struct spider [legs space])
; A Spider is a structure
;   (make-spider Number Number)
;
; interpretation:
; (make-spider n v) represents a spider with `n` legs and that takes up `v` cubic units
; for transportation
(define ex-spider (make-spider 8 27))

(define-struct elephant [space])
; An Elephant is a structure
;   (make-elephant Number)
;
; interpretation:
; (make-elephant v) describes an elephant that takes up `v` cubic units for transportation
(define ex-elephant (make-elephant 100))

(define-struct boa [length girth])
; a Boa is a structure
;   (make-boa Number Number)
;
; interpretation:
; (make-boa l g) describes a boa constrictor of `l` length and `g` girth
(define ex-boa (make-boa 10 3))

(define-struct armadillo [color space])
; An Armadillo is a structure:
;   (make-armadillo String Number)
;
; interpretation:
; (make-armadillo c v) describes an armadillo with a shell of color `c` and a volume of
; `v` cubic units for transportation.
(define ex-armadillo (make-armadillo "yellow" 20))

; A ZooAnimal is one of
; --- Spider
; --- Elephant
; --- Boa
; --- Armadillo

;;; Develop a template for functions that consume zoo animals
;; (define (zoo-fn animal)
;;   (cond [(spider? animal)
;;          (... (spider-legs animal) ... (spider-space animal) ...)]
;;         [(elephant? animal)
;;          (... (elephant-space animal) ...)]
;;         [(boa? animal)
;;          (... (boa-length animal) ... (boa-girth animal) ...)]
;;         [(armadillo? animal)
;;          (... (armadillo-color animal) ... (armadillo-space animal) ...)]))

;;; Design the `fits?` function, which consumes a zoo animal and a description of a cage.
;;; It determines whether the cage's volume is large enough for the animal.

(define-struct cage [length width height])
; A Cage is a structure
;   (make-cage Number Number Number)
;
; interpretation:
; (make-cage length width height) describes a cage of the given dimensions. We assume
; this cage is a rectangular prism.
(define small-cage (make-cage 1 1 1))
(define medium-cage (make-cage 4 4 4))
(define large-cage (make-cage 8 8 8))

; fits: ZooAnimal Cage -> Boolean
; Determines whether `animal` fits in `cage`
(check-expect (fits? ex-spider small-cage)
              (< (spider-space ex-spider) (cage->volume small-cage)))
(check-expect (fits? ex-spider medium-cage) #true)

(check-expect (fits? ex-elephant small-cage)
              (< (elephant-space ex-elephant) (cage->volume small-cage)))
(check-expect (fits? ex-elephant large-cage) #true)

(check-expect (fits? ex-boa medium-cage)
              (< (boa->volume ex-boa) (cage->volume medium-cage)))

(check-expect (fits? ex-armadillo medium-cage)
              (< (armadillo-space ex-armadillo) (cage->volume medium-cage)))
(define (fits? animal cage)
  (cond [(spider? animal)
         (< (spider-space animal) (cage->volume cage))]
        [(elephant? animal)
         (< (elephant-space animal) (cage->volume cage))]
        [(boa? animal)
         (< (boa->volume animal) (cage->volume cage))]
        [(armadillo? animal)
         (< (armadillo-space animal) (cage->volume cage))]))

; fits.v2: ZooAnimal Cage -> Boolean
; Same as above, but we can refactor once we have worked out the templated version of
; the function (!)
(check-expect (fits.v2? ex-spider small-cage)
              (fits? ex-spider small-cage))
(check-expect (fits.v2? ex-spider medium-cage) #true)

(check-expect (fits.v2? ex-elephant small-cage)
              (fits? ex-elephant small-cage))
(check-expect (fits.v2? ex-elephant large-cage) #true)

(check-expect (fits.v2? ex-boa medium-cage)
              (fits? ex-boa medium-cage))

(check-expect (fits.v2? ex-armadillo medium-cage)
              (fits? ex-armadillo medium-cage))
(define (fits.v2? animal cage)
  (< (cond [(spider? animal) (spider-space animal)]
           [(elephant? animal) (elephant-space animal)]
           [(boa? animal) (boa->volume animal)]
           [(armadillo? animal) (armadillo-space animal)])
     (cage->volume cage)))

; cage->volume: Cage -> Number
; computes the volume of a given cage `c`
(define (cage->volume c)
  (* (cage-length c) (cage-width c) (cage-height c)))

; boa->volume: Boa -> Number
; Computes a boa's volume. We treat a boa like a cylinder
(define (boa->volume b)
  (* (/ (* pi (sqr (boa-girth b))) 4)
     (boa-length b)))
