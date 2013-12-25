;;;
;;; Inherited variable functions and fun-ness
;;;
(defvar inherited-variables nil
  "List of variables kepted which are inherited from a \"parent\" buffer")

;;;###autoload
(defun make-variable-buffer-inherited ( a )
  "Make symbol A be inherited -- this implies that it is buffer local.
Also sets the permanent-local property to t so they survive across
mode changes and kill-all-local-variables"
  (add-to-list 'inherited-variables
	       (make-variable-buffer-local a))
  (put a 'permanent-local t))

;;;###autoload
(defun inherit-from-buffer ( buf )
  "Set all inherited variables of current buffer to those values of BUF"
  (interactive "bBuffer: ")
  (let ((curbuf (current-buffer))
	set-list)
    (set-buffer buf)
    (setq set-list
	  (mapcar (lambda ( v )
		    (cons v (symbol-value v)))
		  inherited-variables))
    (set-buffer curbuf)
    (mapcar (lambda ( c )
	      (set (car c) (cdr c)))
	    set-list)))

;;;###autoload
(defun kill-inherited-variables ()
  "Sets all inherited variables to nil"
  (interactive)
  (mapcar (lambda ( v ) (kill-local-variable v)) inherited-variables))

(defadvice get-buffer-create (around inherit activate)
  "Propogates the inherited variables from the current buffer to the
  newly created buffer"
  (let* ((set-list (mapcar (lambda ( v )
			     (cons v (symbol-value v)))
			   inherited-variables)))
    ad-do-it
    (if (buffer-name ad-return-value)
	(with-current-buffer ad-return-value
	  (mapcar (lambda ( c )
		    (set (car c) (cdr c)))
		  set-list)))))

(provide 'inherit)
