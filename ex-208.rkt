#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `boolean-attributes`. The function consumes an LLists and produces the Strings
;;; that are associated with a Boolean attribute.
;;;
;;; HINT: Use `create-set` from exercise 201

;;; Once you are done, determine how many Boolean-valued attributes your iTunes library
;;; employs for its tracks. Do they make sense?

(define ITUNES-LOCATION "itunes.xml")
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; boolean-attributes: LLists -> [Strings]
; Given a list of association lists, produces the string keys for boolean attributes
(check-expect (boolean-attributes '()) '())
(check-expect (boolean-attributes
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane"))
                     (list (list "Artist" "Wham")
                           (list "Members" 2))))
              '())
(check-expect (boolean-attributes
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane")
                           (list "Favorite?" #true))
                     (list (list "Name" "Supper's Ready")
                           (list "Artist" "Genesis")
                           (list "Compressed?" #false))))
              (list "Favorite?" "Compressed?"))
(check-expect (boolean-attributes
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane")
                           (list "Favorite?" #true))
                     (list (list "Name" "Supper's Ready")
                           (list "Artist" "Genesis")
                           (list "Favorite?" #true)
                           (list "Compressed?" #false))))
              (list "Favorite?" "Compressed?"))
(define (boolean-attributes llists)
  (create-set (boolean-attributes/llists llists)))

; boolean-attributes/llists: LLists -> [String]
; Given a list of association lists, produces the string keys for boolean attributes
(check-expect (boolean-attributes/llists '()) '())
(check-expect (boolean-attributes/llists
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane"))
                     (list (list "Artist" "Wham")
                           (list "Members" 2))))
              '())
(check-expect (boolean-attributes/llists
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane")
                           (list "Favorite?" #true))
                     (list (list "Name" "Supper's Ready")
                           (list "Artist" "Genesis")
                           (list "Compressed?" #false))))
              (list "Favorite?" "Compressed?"))
(check-expect (boolean-attributes/llists
               (list (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane")
                           (list "Favorite?" #true))
                     (list (list "Name" "Supper's Ready")
                           (list "Artist" "Genesis")
                           (list "Favorite?" #true)
                           (list "Compressed?" #false))))
              (list "Favorite?" "Favorite?" "Compressed?"))
(define (boolean-attributes/llists llists)
  (cond [(empty? llists) '()]
        [(cons? llists)
         (append (boolean-attributes/lassoc (first llists))
                 (boolean-attributes (rest llists)))]))

; boolean-attributes/lassoc: LAssoc -> [String]
; Returns all keys in `lassoc` for which the value is a boolean
(check-expect (boolean-attributes/lassoc '()) '())
(check-expect (boolean-attributes/lassoc
               (list (list "Name" "Giant Steps")
                     (list "Composer" "John Coltrane")))
              '())
(check-expect (boolean-attributes/lassoc
               (list (list "Name" "Giant Steps")
                           (list "Composer" "John Coltrane")
                           (list "Favorite?" #true)
                           (list "Compressed?" #false)))
              (list "Favorite?" "Compressed?"))
(define (boolean-attributes/lassoc lassoc)
  (cond [(empty? lassoc) '()]
        [(cons? lassoc)
         (if (boolean? (assoc-value (first lassoc)))
             (cons (assoc-key (first lassoc))
                   (boolean-attributes/lassoc (rest lassoc)))
             (boolean-attributes/lassoc (rest lassoc)))]))

; assoc-key: Association -> String
; Given an `assoc`, return its key
(check-expect (assoc-key (list "Key" "Value")) "Key")
(define (assoc-key assoc) (first assoc))

; assoc-value: Association -> BSDN
; Given an `assoc`, return its value
(check-expect (assoc-value (list "Key" "Value")) "Value")
(define (assoc-value assoc) (second assoc))

; create-set: List-of-strings -> List-of-strings
; Given a List-of-strings, `los`, produces a new list, where each element is only there
; exactly once.
;
; assumption: This removes the first occurrence(s) of duplicates
(define (create-set los)
  (cond [(empty? los) '()]
        [(cons? los)
         (if (member? (first los) (rest los))
             (create-set (rest los))
             (cons (first los) (create-set (rest los))))]))

;; (boolean-attributes list-tracks) ; '("Disabled" "Compilation" "Purchased")
