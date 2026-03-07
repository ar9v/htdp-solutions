#lang htdp/isl+

(require 2htdp/batch-io)

;;; Many operating systems come with the `fmt` program, which can rearrange the words in
;;; a file so that all lines in the resulting file have a maximal width. As a widely used
;;; program, `fmt` supports a range of related functions. This exercise focuses on its
;;; core functionality.
;;;
;;; Design the program `fmt`. It consumes a natural number `w`, the name of an input file
;;; `in-f`, and the name of an output file `out-f` -- in the same sense as `read-file`
;;; from the `2htdp/batch-io` teachpack. Its purpose is to read all the words from the
;;; `in-f`, to arrange these words in the given order into lines of maximal width `w`,
;;; and to write these lines to `out-f`.

(define TEST-IN-F "ex-510.in.txt")
(define TEST-OUT-F "ex-510.out.txt")

; Case 1: We split before we've covered a full word
(define split-2-result
  '("The" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dogs"))

; Case 2: We split after a word, but before another one ends
(define split-7-result split-2-result)

; Case 3: We split after two words
(define split-10-result '("The quick" "brown fox" "jumps" "over the" "lazy dogs"))

; Case 4: N spans multiple lines
(define split-26-result
  '("The quick brown fox jumps" "over the lazy dogs"))

; fmt: N String String -> String
; Formats the context of `in-f` into lines of length `len` and writes them to `out-f`
;
; Constraints: `in-f` is the name of a file that exists.
(check-expect (read-lines (fmt 2 TEST-IN-F TEST-OUT-F))
              split-2-result)
(check-expect (read-lines (fmt 7 TEST-IN-F TEST-OUT-F))
              split-7-result)
(check-expect (read-lines (fmt 10 TEST-IN-F TEST-OUT-F))
              split-10-result)
(check-expect (read-lines (fmt 26 TEST-IN-F TEST-OUT-F))
              split-26-result)
(define (fmt len in-f out-f)
  (local [; split:
          ;   N [List-of 1String] [List-of 1String] [List-of String]
          ;      [List-of [List-of String]] -> [List-of [List-of String]]
          (define (split n cs word line lines)
            (cond [(empty? cs)
                   (reverse (cons (reverse (cons (implode (reverse word)) line)) lines))]
                  [(zero? n)
                   (if (<= (- len (length word)) 0)
                       (split len
                              cs
                              word
                              line
                              lines)
                       (split (- len (length word))
                              cs
                              word
                              '()
                              (cons (reverse line) lines)))]
                  [else
                   (local [(define f (first cs))]
                     (cond [(or (string=? f " ") (string=? f "\n"))
                            (split (sub1 n)
                                   (rest cs)
                                   '()
                                   (cons (implode (reverse word)) line)
                                   lines)]
                           [else
                            (split (sub1 n) (rest cs) (cons f word) line lines)]))]))

          (define chars (read-1strings in-f))
          (define lines (split len chars '() '() '()))
          (define out-s (string-join (map (λ (l) (string-join l " ")) lines) "\n"))]
    (write-file out-f out-s)))

; string-join: [List-of String] String -> String
(check-expect (string-join (read-lines TEST-IN-F) "\n")
              "The quick brown fox\njumps over\nthe lazy\ndogs")
(define (string-join strs with)
  (cond [(empty? strs) ""]
        [(empty? (rest strs)) (first strs)]
        [else (string-append (first strs) with (string-join (rest strs) with))]))
