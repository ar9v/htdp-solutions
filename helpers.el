(require 'racket-mode)

(defconst HTDP-DIR (file-name-directory load-file-name)
  "The directory where the HtDP Solutions repo has been cloned and lives in this system.")

;;; an ExFilename is a String that follows the format
;;;
;;;  ex-XXX.rkt
;;;
;;; where XXX are digits.

;;; htdp/exercise-filename?: Any -> Boolean
(defun htdp/exercise-filename? (any)
  "Determines whether any is an ExFilename"
  (and (stringp any) (string-match "ex-[[:digit:]]\\{3\\}.[rkt\\|org]" any)))


(defun htdp/exercise-number (exercise) (car exercise))
(defun htdp/exercise-filename (exercise) (cdr exercise))
;;; an Exercise is a pair: (Number . ExFilename)
;;;
;;; (n . f) represents an exercise in this project, where f is a string with the
;;; absolute filename of the exercise.

;;; htdp/exercise: Number | ExerciseFilename -> Exercise
(defun htdp/exercise (n-or-ex-filename &optional org)
  (cond ((htdp/exercise-filename? n-or-ex-filename)
         (cons (htdp/exercise-filename->number n-or-ex-filename)
               n-or-ex-filename))
        ((numberp n-or-ex-filename)
         (cons n-or-ex-filename
               (htdp/number->exercise-filename n-or-ex-filename org)))))


;;; htdp/exercise-filename->number: ExerciseFilename -> Number
(defun htdp/exercise-filename->number (exercise-filename)
  "Given a filename of form `ex-XXX.rkt`, strips out the XXX and parses it into a number."
  (string-to-number (elt (string-split exercise-filename "[-,.]")
                         1)))

;;; htdp/number->exercise-filename: Number -> ExerciseFilename
(defun htdp/number->exercise-filename (n &optional org)
  "Turns a number `n` into a string of format `ex-XXX.rkt`.

The number is padded w/ 0 if it has less than 3 digits."
  (format "ex-%s.%s"
          (string-pad (number-to-string n) 3 ?0 t)
          (if org "org" "rkt")))

;;; htdp/find-next-exercise: Maybe(Boolean) -> ()
(defun htdp/find-next-exercise (&optional org)
  "`find-file' a file whose filename is whatever comes after the latest exercise.

So, for example, if `HTDP-DIR' has `ex-001.rkt'

(htdp/find-next-exercise) === (find-file ex-002.rkt)

If called with the prefix argument, it'll use .org as an extension instead."

  (interactive "P")

  (find-file (htdp/exercise-filename
              (htdp/exercise (1+ (htdp/exercise-number (htdp/latest-exercise)))
                             org))))

;;; htdp/find-latest-exercise: () -> ()
(defun htdp/find-latest-exercise ()
  "`find-file' the file whose filename is the one with the largest number."
  (interactive)

  (find-file (htdp/exercise-filename (htdp/latest-exercise))))

;;; htdp/find-exercise: Number Maybe(Boolean) -> ()
(defun htdp/find-exercise (n org)
  "HtDP-specific `find-file', which builds the filename. Given `N', it'll build a
filename of the form `ex-%s.rkt', where `%s' will be padded with zeroes accordingly. This
command will fail if run from a directory which is not named `htdp'.

E.g.:

(argv/find-exercise 9)   === (find-file \"ex-009.rkt\")
(argv/find-exercise 11)  === (find-file \"ex-011.rkt\")
(argv/find-exercise 123) === (find-file \"ex-123.rkt\")


There are 528 exercises in the book, so this assumes we'll pad up to 2 spaces.
It doesn't validate that n <= 528, however."

  (interactive "nExercise number: \nP")

  (let ((maybe-exercise-filename (htdp/exercise-filename-for n)))
    (find-file (or maybe-exercise-filename
                   (htdp/exercise-filename (htdp/exercise n org))))))

;;; htdp/exercise-filename-for: Number -> Maybe(ExFilename)
(defun htdp/exercise-filename-for (n)
  "Get the ExerciseFilename that corresponds to number `n', if it exists."
  (alist-get n (htdp/all-exercises)))

;;; htdp/latest-exercise: () -> Maybe(Exercise)
(defun htdp/latest-exercise ()
  "Get the latest Exercise."
  (car (last (htdp/all-exercises))))

;;; htdp/all-exercises: () -> [Exercise]
(defun htdp/all-exercises ()
  "Return a list of all existing Exercises in `HTDP-DIR'"
  (let* ((all-files (sort (project-files (project-current nil HTDP-DIR))))
         (ex-filenames (seq-filter #'htdp/exercise-filename? all-files)))
    (seq-map #'htdp/exercise ex-filenames)))


;;; htdp/region-to-evaluation-steps: Number Number -> ()
(defun htdp/paragraph-to-evaluation-steps ()
  "Turn the code in the paragraph starting from POINT to a series of interactive steps.

That is, given a series of forms FORM1, FORM2 ... up to an empty line

1. Comment them out
2. Stick a comment with == between them"

  (interactive)

  (let ((start (point)))
    (while (< (save-excursion (forward-sexp) (point))
              (save-excursion (search-forward-regexp "^$" nil t)))
      (forward-sexp)
      (insert "\n=="))

    (comment-region start (point))

    ;; Hacky, but kill the last "==" comment
    (kill-whole-line)))


(keymap-set project-prefix-map "h" 'htdp/find-exercise)
(keymap-set project-prefix-map "n" 'htdp/find-next-exercise)
(keymap-set project-prefix-map "l" 'htdp/find-latest-exercise)

(use-package autoinsert
  :config
  (auto-insert-mode 1)

  (setf
   (alist-get '(".*/htdp/ex-[[:digit:]]\\{3\\}\\.rkt" . "HtDP Exercise")
              auto-insert-alist
              nil
              nil
              #'equal)
   '("Lang: "
     "#lang htdp/" str \n
     \n
     (and (yes-or-no-p "Add requires?" )
          '(nil
            "(require 2htdp/image)"    \n
            "(require 2htdp/universe)" \n
            \n))
     ";;; ")))

(define-skeleton htdp/skeleton/define-fn
  "A `define' function form."
  nil
  "(define " "(" (htdp/skeleton-sigil) _ ")" \n
  (htdp/skeleton-sigil) ")")

(define-skeleton htdp/skeleton/cond
  "A `cond' form."
  nil
  "(cond "
  (let ((branches (read-number "# of branches: ")))
    `(,(make-list branches (htdp/skeleton-sigil))
      > "[" str _ "]\n"))

  ;; Hacky, but remove the last newline.
  -1 ")")

(define-skeleton htdp/skeleton/if
  "An `if' form."
  nil
  "(if " (htdp/skeleton-sigil) _ \n
  (htdp/skeleton-sigil) \n
  (htdp/skeleton-sigil) ")")

(define-skeleton htdp/skeleton/bb
  "A `big-bang' form."
  nil
  "(big-bang " (htdp/skeleton-sigil) _ \n
  "[to-draw " (htdp/skeleton-sigil) "]" \n
  "[on-tick " (htdp/skeleton-sigil) "]" \n
  "[on-key " (htdp/skeleton-sigil) "]" \n
  "[stop-when " (htdp/skeleton-sigil) "]" ")")

(define-skeleton htdp/skeleton/ce
  "A `check-expect' form."
  nil
  "(check-expect " \n
  (htdp/skeleton-sigil) _ \n
  (htdp/skeleton-sigil) ")")

(define-skeleton htdp/skeleton/struct
  "A `define-struct' form"
  nil
  "(define-struct " (htdp/skeleton-sigil) _ " " "[" (htdp/skeleton-sigil) "])")

;;; My init.el defines SKELETON-SIGIL, and I use a couple of functions to jump to the
;;; next/previous sigil. Use this at your convenience (-:
(defun htdp/skeleton-sigil ()
  "Return SKELETON-SIGIL if it exists, or a fallback if not"

  (or SKELETON-SIGIL "\"_\""))

(keymap-set racket-mode-map "C-c C-i C-s d" 'htdp/skeleton/define-fn)
(keymap-set racket-mode-map "C-c C-i C-s c" 'htdp/skeleton/cond)
(keymap-set racket-mode-map "C-c C-i C-s i" 'htdp/skeleton/if)
(keymap-set racket-mode-map "C-c C-i C-s b" 'htdp/skeleton/bb)
(keymap-set racket-mode-map "C-c C-i C-s x" 'htdp/skeleton/ce)
(keymap-set racket-mode-map "C-c C-i C-s s" 'htdp/skeleton/struct)
