;; These can be loaded with `M-x load-file` as needed

(defun htdp/find-exercise (n)
  "HtDP-specific `find-file', which builds the filename. Given `N', it'll build a
filename of the form `ex-%s.rkt', where `%s' will be padded with zeroes accordingly. This
command will fail if run from a directory which is not named `htdp'.

E.g.:

(argv/find-exercise 9)   === (find-file \"ex-009.rkt\")
(argv/find-exercise 11)  === (find-file \"ex-011.rkt\")
(argv/find-exercise 123) === (find-file \"ex-123.rkt\")


There are 528 exercises in the book, so this assumes we'll pad up to 2 spaces.
It doesn't validate that n <= 528, however."

  (interactive "nExercise number: " racket-mode)

  (require 'f)

  (if (string= (f-base default-directory) "htdp")
      (let* ((max-digits 3)
             (number-string (number-to-string n))
             (filename (format "ex-%s.rkt" (string-pad number-string max-digits ?0 t))))
          (find-file filename))
    (error "Not in the htdp/ directory!")))

(keymap-set racket-xp-mode-map "C-c C-h" 'htdp/find-exercise)

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
