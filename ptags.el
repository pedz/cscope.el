;;;; In many of my functions, I require a call which determines the
;;;; default system much like tags does.  I use a function from etags
;;;; to do this as well as creating another function which does the
;;;; same thing for non-interactive use.  At some point, I would like
;;;; to amend these functions with a way to modify the syntax table
;;;; temporarily so that the defaults could be better controlled.

(require 'c-syntax)

;;;###autoload
(defun get-symbol-interactively ( prompt &optional history )
  "Used by cscope and others to get a symbol in a tags style interface
and allow for interactive confirmation or changes"
  (let* ((def (thing-at-point 'c-identifier t))
	 (p (if def
		(concat prompt (format "(default %s) " def))
	      prompt)))
    (list (read-string p nil history def))))

;;;###autoload
(defun get-symbol-non-interactively ()
  "Used by cscope and others to get a symbol under the cursor.  Uses
the same mechanism for the default as get-symbol-interactively"
  (thing-at-point 'c-identifier t))

(provide 'ptags)
