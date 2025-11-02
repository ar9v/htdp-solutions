;; These can be loaded with `M-x load-file` as needed

;;; Exercise: (Number . Filename)
(defun htdp/exercise (n-or-ex-filename &optional org)
  (cond ((and (stringp n-or-ex-filename)
              (htdp/exercise-filename? n-or-ex-filename))
         (cons (htdp/exercise-filename->number n-or-ex-filename)
               n-or-ex-filename))
        ((numberp n-or-ex-filename)
         (cons n-or-ex-filename
               (htdp/number->exercise-filename n-or-ex-filename org)))))

(defun htdp/exercise-number (exercise) (car exercise))
(defun htdp/exercise-filename (exercise) (cdr exercise))

(defun htdp/exercise-filename? (string)
  (string-match "ex-[[:digit:]]\\{3\\}.[rkt\\|org]" string))

(defun htdp/exercise-filename->number (exercise-filename)
  (string-to-number (elt (string-split exercise-filename "[-,.]")
                         1)))

(defun htdp/number->exercise-filename (n &optional org)
  (format "ex-%s.%s"
          (string-pad (number-to-string n) 3 ?0 t)
          (if org "org" "rkt")))

(defun htdp/exercise-filename-for (n) (alist-get n (htdp/all-exercises)))
(defun htdp/latest-exercise () (car (last (htdp/all-exercises))))
(defun htdp/all-exercises ()
  (let* ((all-files (sort (project-files (project-current))))
         (ex-filenames (seq-filter #'htdp/exercise-filename? all-files)))
    (seq-map #'htdp/exercise ex-filenames)))

(defun htdp/in-htdp-dir? ()
  "True if the `project-current' is a path with 'htdp' in it."

  (let* ((project (or (project-current) (error "Not in a project")))
         (directory (directory-file-name (project-root project))))

    (string= (car (last (file-name-split directory))) "htdp")))

(defun htdp/find-next-exercise (&optional org)
  (interactive "P")

  (unless (htdp/in-htdp-dir?) (error "Not in HtDP project!"))

  (find-file (htdp/exercise-filename
              (htdp/exercise (1+ (htdp/exercise-number (htdp/latest-exercise)))
                             org))))

(defun htdp/find-latest-exercise ()
  (interactive)

  (unless (htdp/in-htdp-dir?) (error "Not in HtDP project!"))

  (find-file (htdp/exercise-filename (htdp/latest-exercise))))

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
  (require 'f)

  (unless (htdp/in-htdp-dir?) (error "Not in HtDP project!"))

  (let ((maybe-exercise-filename (htdp/exercise-filename-for n)))
    (find-file (or maybe-exercise-filename
                   (htdp/exercise-filename (htdp/exercise n org))))))

(keymap-set project-prefix-map "h" 'htdp/find-exercise)
(keymap-set project-prefix-map "n" 'htdp/find-next-exercise)
(keymap-set project-prefix-map "l" 'htdp/find-latest-exercise)

(defun htdp/insert-evaluation-steps (form)
  "Insert `FORM' into the buffer as a comment, then a newline, and then a separator
and keep prompting for more forms until the empty string is given.

It's a convenience to insert evaluation steps for exercises that ask for how the
DrRacket stepper goes through a program."

  (interactive "sForm: " racket-mode)

  (defun --insert-evaluation-steps ()
    (let ((form (read-string "Form: ")))
      (unless (string= form "")
        (insert ";; ===")
        (newline)
        (insert (format ";; %s" form))
        (newline)
        (--insert-evaluation-steps))))

  (unless (string= form "")
    (insert (format ";; %s" form))
    (newline)
    (--insert-evaluation-steps)))

;;; TODO: use check-expect
(defun htdp/generate-test ()
  (interactive nil racket-mode)

  (let ((bsl-string
         (concat
          (string-join
           (seq-map
            (lambda (bsl-exprs)
              (format "(check-expect %s)" bsl-exprs))
            (htdp/functional-examples->bsl-exprs
             (htdp/functional-examples-in-region (region-beginning)
                                                 (region-end))))
           "\n"))))
    (goto-char (point-max))
    (insert bsl-string)

    ;; TODO: make these last steps prettier/less hacky
    (mark-sexp -1)
    (indent-for-tab-command)))

(defun htdp/functional-example->bsl-expr (functional-example)
  (format "(%s %s) %s"
          (htdp/defined-function-in-region)
          (car functional-example)
          (cdr functional-example)))

(defun htdp/functional-examples->bsl-exprs (functional-examples)
  (seq-map
   #'htdp/functional-example->bsl-expr
   functional-examples))

(defun htdp/functional-examples-in-region (start end)
  (seq-filter
   #'identity
   (seq-map
    (lambda (s)
      (when (string-match "given: \\(.*\\), expect: \\(.*\\)" s)
        (cons (match-string 1 s) (match-string 2 s))))
    (string-lines (buffer-substring-no-properties start end)))))

(defun htdp/defined-function-in-region ()
  (let ((text-in-region (buffer-substring-no-properties (region-beginning) (region-end))))
    (string-match
     "(define (\\([[:graph:]]+\\)"       ; TODO: figure out a fancier way of doing this
     text-in-region)

    (match-string 1 text-in-region)))
