#lang htdp/bsl

;;; Create templates for functions that consume instances of the following structure types:

(define-struct movie [title director year])
;; (define (movie-fn movie)
;;   (... (movie-title movie) ... (movie-director movie) ... (movie-year movie) ...))

(define-struct pet [name number])
;; (define (pet-fn pet)
;;   (... (pet-name pet) ... (pet-number pet) ...))

(define-struct CD [artist title price])
;; (define (CD-fn cd)
;;   (... (CD-artist cd) ... (CD-title cd) ... (CD-price cd) ...))

(define-struct sweater [material size color])
;; (define (sweater-fn s)
;;   (... (sweater-material s) ... (sweater-size s) ... (sweater-color s) ...))
