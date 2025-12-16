#lang htdp/bsl

(require 2htdp/batch-io)

;;; You know what the data definition for List-of-strings looks like. Spell it out.

; A List-of-strings is one of
; -- '()
; -- (cons String List-of-strings)


;;; Make sure that you can represent Piet Hein's poem as an instance of the definition
;;; where each line is represented as a string...

(define poem-as-list-of-strings
  (cons
   "TTT"
   (cons
    ""
    (cons
     "Put up in a place"
     (cons
      "where it's easy to see"
      (cons
       "the cryptic admonishment"
       (cons
        "T.T.T."
        (cons
         ""
         (cons "When you feel how depressingly"
               (cons "slowly you climb,"
                     (cons "it's well to remember that"
                           (cons "Things Take Time."
                                 (cons ""
                                       (cons "Piet Hein" empty))))))))))))))

;;; ... and another instance where each word is a string.

;;; I did not write this out by hand; I used Emacs macros (before anyone thinks I have gone
;;; mad)
(define poem-as-list-of-words
  (cons
   "TTT"
   (cons
    "Put"
    (cons
     "up"
     (cons
      "in"
      (cons
       "a"
       (cons
        "place"
        (cons
         "where"
         (cons
          "it's"
          (cons
           "easy"
           (cons
            "to"
            (cons
             "see"
             (cons
              "the"
              (cons
               "cryptic"
               (cons
                "admonishment"
                (cons
                 "T.T.T."
                 (cons
                  "When"
                  (cons
                   "you"
                   (cons
                    "feel"
                    (cons
                     "how"
                     (cons
                      "depressingly"
                      (cons
                       "slowly"
                       (cons
                        "you"
                        (cons
                         "climb,"
                         (cons
                          "it's"
                          (cons
                           "well"
                           (cons
                            "to"
                            (cons
                             "remember"
                             (cons
                              "that"
                              (cons
                               "Things"
                               (cons
                                "Take"
                                (cons
                                 "Time."
                                 (cons
                                  "Piet"
                                  (cons "Hein" '()))))))))))))))))))))))))))))))))))


;;; Use `read-lines` and `read-words` to confirm your representation choices.
(check-expect poem-as-list-of-strings (read-lines "ttt.txt"))
(check-expect poem-as-list-of-words (read-words "ttt.txt"))


;;; Next develop the data definition for List-of-list-of-strings. Again, represent
;;; Piet Hein's poem as an instance of the definition where each line is represented as
;;; a list of strings, one per word, and the entire poem is a list of such line
;;; representations. You may use `read-words/line` to confirm your choice.

; A List-of-list-of-strings is one of
; -- '()
; -- (cons List-of-string List-of-list-of-strings)

(define poem-as-lolos
  (cons
   (cons "TTT" empty)
   (cons
    empty
    (cons
     (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" empty)))))
     (cons
      (cons "where" (cons "it's" (cons "easy" (cons "to" (cons "see" empty)))))
      (cons
       (cons "the" (cons "cryptic" (cons "admonishment" empty)))
       (cons
        (cons "T.T.T." empty)
        (cons
         empty
         (cons
          (cons "When" (cons "you" (cons "feel" (cons "how" (cons "depressingly" empty)))))
          (cons
           (cons "slowly" (cons "you" (cons "climb," empty)))
           (cons
            (cons "it's" (cons "well" (cons "to" (cons "remember" (cons "that" empty)))))
            (cons
             (cons "Things" (cons "Take" (cons "Time." empty)))
             (cons
              empty
              (cons
               (cons "Piet" (cons "Hein" empty))
               empty))))))))))))))

(check-expect poem-as-lolos (read-words/line "ttt.txt"))
