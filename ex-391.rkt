#lang htdp/isl+

;;; Design `replace-eol-with` using the strategy of chapter 23.3. Start from the tests.
;;; Simplify the result systematically.

; replace-eol-with: [List-of Any] [List-of Any] -> [List-of Any]
; replaces the final '() in `front` with `end`
(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '(1 2 3) '()) '(1 2 3))
(check-expect (replace-eol-with '() '(4 5 6)) '(4 5 6))
(check-expect (replace-eol-with '(1 2) '(3 4 5)) '(1 2 3 4 5))
(define (replace-eol-with front end)
  (if (empty? front)
      end
      (cons (first front) (replace-eol-with (rest front) end))))

; (define (replace-eol-with front end)
;   (cond [(and (empty? front) (empty? end)) '()]
;         [(and (empty? front) (cons? end)) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
;
; In the first branch, `end` is '(), so we can substitute it
; (define (replace-eol-with front end)
;   (cond [(and (empty? front) (empty? end)) end]
;         [(and (empty? front) (cons? end)) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
;
; Since the first both branches produce the same result, we can collapse them into a single
; branch.
; (define (replace-eol-with front end)
;   (cond [(empty? front) end]
;         [(and (cons? front) (empty? end)) front]
;         [(and (cons? front) (cons? end))
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
;
; Since `(cons? front)` is always true for the remaining branches, we can forego it.
; (define (replace-eol-with front end)
;   (cond [(empty? front) end]
;         [(empty? end) front]
;         [(cons? end)
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
;
; When `end` is non-empty, we recurse. In that case, we'll run `front` out and wind up with
; `end`. If `end` is empty, we return `front`, which is the same result we'd arrive at
; by recursing, since we pass `end` as-is. So we can collapse both cases into `else`.
; (define (replace-eol-with front end)
;   (cond [(empty? front) end]
;         [else
;          (cons (first front)
;                (replace-eol-with (rest front) end))]))
;
; Now we just translate the `cond` to `if`.
; (define (replace-eol-with front end)
;   (if (empty? front)
;       end
;       (cons (first front) (replace-eol-with (rest front) end))))
