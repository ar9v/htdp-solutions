#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/batch-io)

;;; Here is the `get` function.
;;;
;;; It assumes the existence of `get-xexpr`, a function that searches an arbitrary Xexpr.v3
;;; for the desired attribute and produces [Maybe String].
;;;
;;; Formulate test cases that look for other values than "F" and that force `get` to signal
;;; an error.

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute from a 'meta element that has attribute
; "itemprop" with value `s`
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-error (get '(meta ((content "+1") (itemprop "F"))) "G"))
(check-error (get (read-xexpr "ford-wayback.html") "fakeProp"))
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

;;; Design `get-xexpr`. Derive functional examples for this function from those for `get`.
;;; Generalize these examples so that you are confident that `get-xexpr` can traverse an
;;; arbitrary Xexpr.v3. Finally, formulate a test that uses the web data saved in exercise
;;; 385.

; get-xexpr: Xexpr.v3 String -> [Maybe String]
; retrieves the value of the "content" attribute from an element in `xexpr` that has
; attribute "itemprop" with value `s`
;
; Assumptions:
; -- `itemprop` values are unique
; -- Xexprs with "itemprop" attributes also have a "content" attribute
(check-expect (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F") "+1")
(check-expect (get-xexpr '(html
                           (head
                            (meta ((content "+2") (itemprop "G")))
                            (meta ((content "+1") (itemprop "F")))))
                         "F")
              "+1")
(check-satisfied (get-xexpr (read-xexpr "ford-wayback.html") "priceChange") string?)
(check-expect (get-xexpr '(meta ((content "+1") (itemprop "F"))) "G") #false)
(check-expect (get-xexpr (read-xexpr "ford-wayback.html") "fakeProp") #false)
(define (get-xexpr x s)
  (local [(define itemprop (find-attr 'itemprop (xexpr-attr x)))
          (define (get-xexprs xs s) (for/or [(x xs)] (get-xexpr x s)))]
    (cond [(or (symbol? x) (string? x) (number? x)) #false]
          [else
           (if (and (string? itemprop) (string=? itemprop s))
               (find-attr 'content (xexpr-attr x))
               (get-xexprs (xexpr-content x) s))])))

; xexpr-attr: Xexpr.v3 -> [List-of Attribute]
(define (xexpr-attr xe)
  (cond [(or (symbol? xe) (string? xe) (number? xe)) '()]
        [else
         (local ((define optional-loa+content (rest xe)))
           (cond
             [(empty? optional-loa+content) '()]
             [else
              (local ((define loa-or-x
                        (first optional-loa+content)))
                (if (list-of-attributes? loa-or-x)
                    loa-or-x
                    '()))]))]))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(define (xexpr-content xe)
  (cond [(or (symbol? xe) (string? xe) (number? xe)) '()]
        [else
         (match xe
           [(list (? symbol?)) '()]
           [(list (? symbol?) (? list-of-attributes?)) '()]
           [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
           [(cons (? symbol?) (cons x xs)) (cons x xs)]
           [other (error "Error: " other " is not a valid XML representation")])]))

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))

; find-attr: [List-of Attributes] Symbol -> [Maybe String]
; Given `attrs`, looks up the value for attribute `attr`, if it exists; returns #false
; if it doesn't exist.
(define (find-attr attr attrs)
  (match (assoc attr attrs)
    [(list _attr value) value]
    [#false #false]))
