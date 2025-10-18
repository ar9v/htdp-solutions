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
