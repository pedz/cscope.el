

(defgroup c-syntax nil
  "C syntax patterns and mathers"
  :group 'languates)

(defcustom c-identifier-first-character "a-zA-Z_"
  "Regular expression for the acceptable first character of a C
identifier"
  :group 'c-syntax
  :type 'regexp)

(defcustom c-identifier-non-first-character "a-zA-Z_0-9"
  "Regular expression for the acceptable characters of a C identifier
after the first character"
  :group 'c-syntax
  :type 'regexp)

(defcustom c-keywords
  (concat "\\`\\("
	  "auto"     "\\|"
	  "break"    "\\|"
	  "case"     "\\|"
	  "char"     "\\|"
	  "continue" "\\|"
	  "default"  "\\|"
	  "do"       "\\|"
	  "double"   "\\|"
	  "else"     "\\|"
	  "enum"     "\\|"
	  "extern"   "\\|"
	  "float"    "\\|"
	  "for"      "\\|"
	  "goto"     "\\|"
	  "if"       "\\|"
	  "int"      "\\|"
	  "long"     "\\|"
	  "register" "\\|"
	  "return"   "\\|"
	  "short"    "\\|"
	  "signed"   "\\|"
	  "sizeof"   "\\|"
	  "static"   "\\|"
	  "struct"   "\\|"
	  "switch"   "\\|"
	  "typedef"  "\\|"
	  "union"    "\\|"
	  "unsigned" "\\|"
	  "void"     "\\|"
	  "while"
	  "\\)\\'" )
  "A regular expression that matches C keywords"
  :group 'c-syntax
  :type 'regexp)

(defun c-identifier-bounds ()
  "True if point is immediately before or within a C identifier"
  (message "here!")
  (if (looking-at (concat "[" c-identifier-non-first-character "]"))
      (save-match-data
	(save-excursion
	  (skip-chars-backward c-identifier-non-first-character)
	  (if (re-search-forward
	       (concat "\\="
		       "[" c-identifier-first-character "]"
		       "[" c-identifier-non-first-character "]*")
	       nil t)
	      (let* ((start (match-beginning 0))
		     (end (match-end 0))
		     (string (buffer-substring start end)))
		(if (not (string-match c-keywords string))
		    (cons start end))))))))

(put 'c-identifier 'bounds-of-thing-at-point 'c-identifier-bounds)

(provide 'c-syntax)
