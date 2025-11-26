#lang htdp/bsl

;;; Your home town manages a fleet of vehicles: automobiles, vans, buses, and SUVs.
;;; Develop a data representation for vehicles. The representation of each vehicle must
;;; describe the number of passengers that it can carry, its license plate number, and
;;; its fuel consumption (miles per gallon).

(define AUTO "auto")
(define VAN "van")
(define BUS "bus")
(define SUV "suv")
; A VehicleType is one of
; --- AUTO
; --- VAN
; --- BUS
; --- SUV

(define-struct vehicle [type passengers license-plate fuel-consumption])
; A Vehicle is a structure
;   (make-vehicle VehicleType Number String Number)
;
; interpretation:
; (make-vehicle type passengers license-plate fuel-consumption) represents a vehicle
; of type `type` with a number of passengers `passengers`, for which the license plate
; is `license-plate` and that consumes `fuel-consumption` miles per gallon.
(define automobile (make-vehicle AUTO 5 "SYX-404" 20))
(define suv (make-vehicle SUV 5 "SYY-505" 15))
(define van (make-vehicle VAN 8 "SET-101" 18))
(define bus (make-vehicle BUS 30  "YYY-333" 10))

;;; Develop a template for functions that consume vehicles.
;; (define (vehicle-fn v)
;;   (... (vehicle-type v)
;;    ... (vehicle-passengers v)
;;    ... (vehicle-license-plate v)
;;    ... (vehicle-fuel-consumption v)))
