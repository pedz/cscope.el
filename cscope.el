;;; cscope.el --- emacs interface to cscope

;; This is a ground up review and rewrite of code that I originally
;; wrote while contracting at Tandem (in Austin).
;; Tue Dec  3 08:25:25 CST 2013

;; This was written by Perry Smith (a.k.a. pedz) at Tandem 
;; Thu Jul 26 10:32:36 CDT 1990

;; I work in an environment that runs an old copy of cscope but other
;; versions of cscope exist at the same time.  The environment also
;; has pre-built databases for different cross sections of the files.
;; Thus, the cscope database is rarely called 'cscope.out' nor is it
;; usually at the top of the source tree.  I also work in support so I
;; am frequently looking at different versions of the same file at the
;; same time.  The features of this cscope.el are to address these
;; needs.
;; 
;; The first feature of this cscope start with using `inherit' which
;; is a package that introduces the concept of inherited buffer local
;; variables see `make-variable-buffer-inherited'.  When buffer 2 is
;; create while in buffer 1, the inherited variables of buffer 2 are
;; initialized from the current values of buffer 1.  This provides the
;; ability to have groups of buffers that know their ancestary which
;; in tern allows to have multiple cscopes running at the same time
;; and the buffers created from one cscope know to use the cscope for
;; that buffer.  For example, a user can be looking at code in version
;; A and version B at the same time and the cscope searches know that
;; if searches while visiting a file from version A to use the cscope
;; running for version A.  The hook to do this is tied in to
;; `get-buffer-create' so a `grep' or `dired' done while visiting a
;; file from version A still knows to go back to the cscope for
;; version A if a subsequent search is done.
;;
;; The second feature is the cscope executable, the options to pass to
;; cscope, the top level directory of where the cscope thinks it was
;; cretaed, and the full path to the cscope database (and thus its
;; name) are four separate values are easily set for each cscope that
;; is started.  The user can create patterns that match the directory
;; of the file being edited and thus determine defaults for the four
;; values.  The user can also create functions such as "cscope-happy"
;; that starts the cscope for the Happy project without further
;; interaction.
;;
;; There is one "inherited" buffer which is `cscope-obarray'.  All the
;; buffers that should use a particular cscope process will have their
;; buffer local value of `cscope-obarray' pointing to the same
;; obarray.  This is accomplished through the `inherit' package.
;; Within `cscope-obarray' is a set of symbols (variables) that are
;; assigned when the cscope process is started.  One example is the
;; variable `cscope-out-buffer' which will not exist in the global
;; obarray but will be in `cscope-obarray'.  Three functions will be
;; created (in the global obarray): the function `cscope-out-buffer'
;; which will return the symbol interned in `cscope-obarray', the
;; function `cscope-out-buffer-get' which will return the value of the
;; symbol and `cscope-out-buffer-set' which will set the value of the
;; symbol.
;;
;; I think of `cscope-obarray' as an instance of an object and the
;; variables in it as attributes within the object.  All the buffers
;; using the same set of values for the cscope process, output buffer,
;; etc will point to the same `cscope-obarray' "instance".

(require 'inherit)
(require 'ptags)

;; First the customizable group and variables
(defgroup cscope-mode nil
  "CScope mode"
  :group 'languages)

;; ToDo: The first two patterns need to be removed and put into a
;; separate GSA cscope project along with a method of finding the
;; various backing trees on GSA and creating convenience functions to
;; starting a cscope for a specific backing tree or release.
(defcustom cscope-dir-patterns
  (list
   ;; Pattern that always matches
   '("."
     "cscope"
     "-d -q -l"
     (cscope-walk-tree "cscope.out")
     (concat (cscope-walk-tree "cscope.out") "cscope.out"))
   )
  "A list of pattern elements.  Each pattern element is itself a
list containing five elements ( DIRMATCH CSCOPE OPTIONS DIR DATABASE

DIRMATCH is a regular expressions that is matched against the
`default-directory' of the buffer the cscope search is attempted
from.  The remaining four elements are lisp expressions whose values
is used to set the value of variable `cscope-process-cscope', variable
`cscope-process-options', variable `cscope-process-dir', and variable
`cscope-process-database' respectively.  Note that `string-match is
used to match DIRMATCH with the `default-directory' so `match-string'
may be used in the lisp expressions"
  :group 'cscope-mode
  :type 'list)

(defcustom cscope-auto-go t
  "When only 1 cscope entry is found, emacs automatically selects that
entry"
  :group 'cscope-mode
  :type 'boolean)

(defcustom cscope-key-command-prefix (kbd "C-\\")
  "The prefix for the cscope key bindings"
  :type 'string
  :group 'cscope-mode)

(defcustom cscope-mark-ring-max 16
  "Number of markers to keep in `cscope-mark-ring'"
  :type 'integer
  :group 'cscope-mode)

;; We define one inhertied variable which is an obarray that will
;; contain the half dozen or so variables needed to drive cscope.
(defvar cscope-obarray nil
  "Obarray containing per cscope instance variables")
(make-variable-buffer-inherited 'cscope-obarray)

(defvar cscope-mode-menu nil
  "The Cscope menu")

(defvar cscope-mode-map
  (let* ((parent (make-sparse-keymap))
	 (child (make-sparse-keymap)))
    (easy-menu-define cscope-mode-menu
      parent
      "Menu used when `cscope-mode' is active."
      '("CScope"
	["Find references" cscope-menu-find-references 
	 :help "Find references to the SYMBOL at point"]
	["Find declarations" cscope-menu-find-declarations 
	 :help "Find declarations for the SYMBOL at point"]
	["Find functions called" cscope-menu-find-functions-called 
	 :help "Find functions called by SYMBOL at point"]
	["Find calls to" cscope-menu-find-calling-functions 
	 :help "Find calls to SYMBOL at point"]
	["Find STRING" cscope-menu-find-string 
	 :help "Find STRING at point"]
	["Find rexexp" cscope-menu-find-pattern 
	 :help "Find regular expression PATTERN at point"]
	["Find files" cscope-menu-find-file 
	 :help "Find files matching PATTERN at point"]
	["Find includes" cscope-menu-find-file-include 
	 :help "Find files which includes files matching PATTERN at point"]
	["Find assignments" cscope-menu-find-assignment 
	 :help "Find assignments to SYMBOL at point"]
	"--"
	["Find references ..." cscope-find-references 
	 :help "Find references to the SYMBOL interactively"]
	["Find declarations ..." cscope-find-declarations 
	 :help "Find declarations for the SYMBOL interactively"]
	["Find functions called ..." cscope-find-functions-called 
	 :help "Find functions called by SYMBOL interactively"]
	["Find calls to ..." cscope-find-calling-functions 
	 :help "Find calls to SYMBOL interactively"]
	["Find STRING ..." cscope-find-string 
	 :help "Find STRING interactively"]
	["Find regexp ..." cscope-find-pattern 
	 :help "Find regular expression PATTERN interactively"]
	["Find files ..." cscope-find-file 
	 :help "Find files matching PATTERN interactively"]
	["Find includes ..." cscope-find-file-include 
	 :help "Find files which includes files matching PATTERN interactively"]
	["Find assignments ..." cscope-find-assignment 
	 :help "Find assignments to SYMBOL interactively"]
	))
    (define-key child (kbd "F") 'cscope-find-file)
    (define-key child (kbd "c") 'cscope-find-calling-functions)
    (define-key child (kbd "f") 'cscope-find-declarations)
    (define-key child (kbd "i") 'cscope-find-file-include)
    (define-key child (kbd "n") 'cscope-next-mark)
    (define-key child (kbd "p") 'cscope-previous-mark)
    (define-key child (kbd "s") 'cscope-find-references)
    (define-key parent cscope-key-command-prefix child)
    parent)
  "Keymap used for cscope mode")

(defvar cscope-result-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map " " 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\177" 'previous-line)
    (define-key map "v" 'cscope-view-from-list)
    (define-key map "q" 'delete-window)
    (define-key map "e" 'cscope-goto-from-list)
    (define-key map [mouse-1] 'cscope-mouse-goto-from-list-other-window)
    (define-key map [drag-mouse-1] 'cscope-mouse-no-op)
    (define-key map [down-mouse-1] 'cscope-mouse-no-op)
    (define-key map [mouse-2] 'cscope-mouse-goto-from-list)
    (define-key map [drag-mouse-2] 'cscope-mouse-no-op)
    (define-key map [down-mouse-2] 'cscope-mouse-no-op)
    (define-key map [mouse-3] 'cscope-mouse-view-from-list)
    (define-key map [drag-mouse-3] 'cscope-mouse-no-op)
    (define-key map [down-mouse-3] 'cscope-mouse-no-op)
    map)
  "Keymap used in cscope mode.")

;; The variables that cscope uses are stored away in `cscope-obarray'.
;; This macro provides a way to define the variable (interned within
;; `cscope-obarray') as well as convenient getter and setter methods.
(defmacro cscope-defvar (name init doc)
  "Defines three functions.  One returns the symbol NAME within
the current `cscope-obarray' giving it an initial value of INIT
and a documentation string of DOC.  The other two are getter and
setter convenience methods."
  (declare (indent defun))
  (let ((name-string (format "%s" name))
	(sym-name (intern (format "%s-sym" name)))
	(sym-doc (format
		 "Retrieves the symbol `%s' from `cscope-obarray'."
		 name))
	(getter-name (intern (format "%s-get" name)))
	(getter-doc (format
		 "Retrieves the value of the variable `%s' from `cscope-obarray'."
		 name))
	(setter-doc (format
		 "Sets the variable `%s' within `cscope-obarray'."
		 name))
	(setter-name (intern (format "%s-set" name))))
    `(progn
       (defun ,sym-name ()
	 ,sym-doc
	 (let ((sym (intern ,name-string cscope-obarray)))
	   (unless (boundp sym)
	     (put sym 'variable-documentation ,doc)
	     (set sym ,init))
	   sym))
       (defun ,getter-name ()
	 ,getter-doc
	 (symbol-value (,sym-name)))
       (defun ,setter-name (val)
	 ,setter-doc
	 (set (,sym-name) val)))))

(cscope-defvar cscope-out-buffer nil
  "The cscope output buffer.")

(cscope-defvar cscope-process nil
  "The cscope process.")

(cscope-defvar cscope-process-cscope "cscope"
  "Path to the cscope executable.")

(cscope-defvar cscope-process-options "-q -d"
  "Options to pass to cscope in addition to the -P path and -f path
options.  A '-l' is added to the list since that must always be
used.")

(cscope-defvar cscope-process-dir nil
  "The directory to specify with the -P option when starting the
cscope process.  This is the top level directory of the source tree
passed to cscope when the database was built")

(cscope-defvar cscope-process-database nil
  "The full path to the cscope database which is passed to the cscope
process via the -f option")

(cscope-defvar cscope-process-start-time nil
  "The file modification time of the cscope.out file when
the cscope process was started.")

(cscope-defvar cscope-file-vector nil
  "Holds the full path names for the files listed in the cscope output")

(cscope-defvar cscope-line-vector nil
  "Holds the line numbers for the lines listed in the cscope output")

(cscope-defvar cscope-history nil
  "Holds the history of the searches made for this cscope")

(cscope-defvar cscope-mark-ring nil
  "Ring of locations much like `global-mark-ring' to help
navigate back to previous places in your cscope editing
history.")

(cscope-defvar cscope-mark-nth -1
  "Current index into `cscope-mark-ring'.  `cscope-previous-mark'
goes to the mark at `cscope-mark-nth' position and then
increments it by one.  `cscope-next-mark' decrements it by one
and then goes to the marker.  The list does not wrap.
`cscope-previous-mark' will error if `cscope-mark-nth' is past
the end of the list.  `cscope-next-mark' will error if
`cscope-mark-nth' is already at the front of the list.")

(cscope-defvar cscope-flush-patterns nil
  "Can be set to a list of regular expressions.  `cscope-format' will
delete lines (cscope hits) that match any of the patterns before
making the final list of cscope hits.")

(defun cscope-clear-flush-patterns ()
  "Sets `cscope-flush-patterns' to nil"
  (interactive)
  (cscope-flush-patterns-set nil))

(defun cscope-append-flush-patterns ( patterns )
  "Appends PATTERNS to `cscope-flush-patterns'.  PATTERNS may be a
  single string or a list of strings"
  (interactive "sRegexp string: ")
  (cscope-flush-patterns-set (append (cscope-flush-patterns-get)
				     (or (and (listp patterns) patterns)
					 (list patterns)))))

(defun cscope-needs-to-be-started ()
  "Returns true if the cscope has never been started, has died and
  needs to be restarted, or if the database has been updated implying
  that the cscope process needs to be killed and then restarted."
  (let* (process database file-mod-time buffer)
    (not (and
	  (setq buffer (cscope-out-buffer-get))
	  (bufferp buffer)						;cscope-out-buffer has been set
	  (buffer-name buffer)						;cscope-out-buffer still live
	  (setq process (get-buffer-process buffer)) 			;has a process
	  (eq process (cscope-process-get))				;same process as before
	  (eq (process-status process) 'run)				;process still running
	  (setq database (cscope-process-database-get))			;database path
	  (setq file-mod-time (nth 5 (file-attributes database)))	;get database mod time
	  (equal (cscope-process-start-time-get) file-mod-time)))))	;mod time hasn't changed

;; Stolen from rspec-mode.el
(defun cscope-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

;; Stolen from rspec-mode.el
(defun cscope-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (cscope-parent-directory a-directory)))

(defun cscope-walk-tree ( f )
  "Walk up directory tree starting from `default-directory' looking
for a file named FILE.  Returns the directory FILE is found in or nil
if FILE is not found."
  (let ((dir default-directory))
    (while (and (not (cscope-root-directory-p dir))
		(not (file-exists-p (concat dir f))))
      (setq dir (cscope-parent-directory dir)))
    (if (file-exists-p (concat dir f))
	dir
      nil)))

(defun cscope-get-or-create-out-buffer ()
  "Returns `cscope-out-buffer' after validating it.
If `cscope-needs-to-be-started', then a new cscope process is kicked
off using `cscope-dir-patterns' to determine the values to use to
start the cscope process.  If no match is found in
`cscope-dir-patterns', an error is signaled."
  (if (cscope-needs-to-be-started)
      (let ((var cscope-dir-patterns)
	    temp)
	(while (not (or (null var)
			(string-match (nth 0 (nth 0 var)) default-directory)))
	  (setq var (cdr var)))
	(if var
	    (progn
	      (setq temp (car var))
	      (message "cscope-get-or-create-out-buffer %s %s %s %s"
		       (eval (nth 1 temp))
		       (eval (nth 2 temp))
		       (eval (nth 3 temp))
		       (eval (nth 4 temp)))
	      (cscope-init-process
	       (eval (nth 1 temp))
	       (eval (nth 2 temp))
	       (eval (nth 3 temp))
	       (eval (nth 4 temp))))
	  ;; ToDo: Currently, the only way to start cscope is to open
	  ;; a source file and then search for a symbol.  The code
	  ;; will automatically find the cscope database or hit this
	  ;; error if it can't figure it out.  Convenience functions
	  ;; need to be created so a user can start a cscope
	  ;; anywhere and with a user's selected set of cscope,
	  ;; options, directory, and database.
	  (error "`cscope-dir-patters' did not match current directory"))))
  (cscope-out-buffer-get))

(defun cscope-buffer-name (cscope options dir database)
  "Returns the name of cscope out buffer.
Given CSCOPE, OPTIONS, DIR, and DATABASE, the name for the
`cscope-out-buffer' is returned."
  (format "*%s %s*"
	  (file-name-nondirectory (directory-file-name (expand-file-name dir)))
	  (file-name-nondirectory (directory-file-name (expand-file-name database)))))

(defun cscope-get-buffer-create (name)
  "If a new buffer is created, a new cscope-obarray is also created.
The new obarray is created in the exiting buffer and will be inherited
by the new buffer."
  (let ((buffer (get-buffer name)))
    (unless buffer
      (setq cscope-obarray (make-vector 3 0))
      (setq buffer (get-buffer-create name))
      (buffer-disable-undo buffer))
    buffer))

(defun cscope-init-process ( cscope options dir database )
  "Initialize a cscope process using CSCOPE OPTIONS DIR and DATABASE.
This is not an interactive command.  The intent is interactive
commands will be wrapped around this function.  The buffer for the
process will be what `cscope-buffer-name' returns when passed the same
four args passed to this function.  If the buffer already exists and
if the process is still running, a new buffer and new process is not
created."
  (message "cscope-init-process called: cscope='%s' options='%s' dir='%s' database='%s'"
   	   cscope options dir database)
  (setq dir (expand-file-name dir)
	database (expand-file-name database))
  (let* ((buf (cscope-get-buffer-create (cscope-buffer-name cscope options dir database)))
	 (process (get-buffer-process buf))
	 (file-mod-time (nth 5 (file-attributes database)))
	 (dead-process (or (null process)
			(null (eq (process-status process) 'run))))
	 (options-list (append 
			(save-match-data (split-string options))
			(list
			 "-P"
			 dir
			 "-f"
			 database))))

    ;; Kill current process if the cscope.out file has been modified.
    ;; De Morgan's equivalent to:
    ;; if process exists AND it is not dead AND the modification time
    ;;    do not match, then
    (unless (or (not process)
		dead-process
		(equal (cscope-process-start-time-get) file-mod-time))
      (message "Killing cscope process %s %s %s %s %s"
	       (not process)
	       dead-process
	       (equal (cscope-process-start-time-get) file-mod-time)
	       (cscope-process-start-time-get)
	       file-mod-time)
      (kill-process process)
      (with-current-buffer buf
	(setq buffer-read-only nil)
	(accept-process-output process))
      (setq dead-process t))

    ;; If process is nil or dead, we need to start the process.
    (message "dead-process is %s" dead-process)
    (if dead-process
	(progn
	  (if process
	      (message "Restarting dead cscope-process... %s %s" cscope options-list)
	    (message "Starting new cscope process... %s %s" cscope options-list))
	  (set-process-query-on-exit-flag
	   (setq process (apply 'start-process "cscope" buf
				cscope
				options-list)) nil)

	  ;; for the new buffer, set the cscope specific variables
	  ;; within the new buffer.  Note that cscope-out-buffer in
	  ;; buf points to itself (the new buffer)
	  (cscope-out-buffer-set buf)
	  (message "setting process to %d" (process-id process))
	  (cscope-process-set process)
	  (cscope-process-cscope-set cscope)
	  (cscope-process-options-set options)
	  (cscope-process-dir-set dir)
	  (cscope-process-database-set database)
	  (cscope-process-start-time-set file-mod-time)
	  (with-current-buffer buf
	    (setq default-directory dir)
	    (cscope-wait)

	    ;; This is a bit silly but I want the buffer to look nice
	    ;; when first started.
	    (erase-buffer)
	    (setq buffer-read-only t)
	    (set-buffer-modified-p nil)

	    ;; Now, go into cscope-result-mode
	    (cscope-result-mode))))))

;;;###autoload
(define-minor-mode cscope-mode
  "Minor mode for C files that use cscope

\\{cscope-mode-map}"
  :lighter " Cscope")

(define-derived-mode cscope-result-mode nil "C-scope"
  "mode the `cscope-out-buffer' is set to.

\\{cscope-result-mode-map}"
  (cscope-mode)
  (setq truncate-lines t))

(defun cscope-format ()
  "Formats the output of cscope to be pretty.
Return value is the number of lines generated.
`current-buffer' must be set to the proper `cscope-out-buffer' before
being called."
  (let ((longest-file 0)
	(longest-function 0)
	(longest-line 0)
	(counter 0)
	(flush-patterns (cscope-flush-patterns-get))
	pat return-value)

    ;; delete the ">> " last line
    (goto-char (point-max))
    (beginning-of-line)
    (delete-region (point)
		   (save-excursion
		     (forward-line 1)
		     (point)))

    ;; delete the "cscope: 2 lines"
    (goto-char (point-min))
    (delete-region (point)
		   (save-excursion
		     (forward-line 1)
		     (point)))

    ;; flush the lines that the user doesn't want to see.
    (dolist (pattern flush-patterns)
      (flush-lines pattern)
      (goto-char (point-min)))

    ;;
    ;; Go through buffer finding the longest filename, function name,
    ;; and line number.
    ;; 
    (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)"
			      nil t)
      (let ((filename (file-name-nondirectory
		       (buffer-substring (match-beginning 1) (match-end 1))))
	    (function (buffer-substring (match-beginning 2) (match-end 2)))
	    (linenum (buffer-substring (match-beginning 3) (match-end 3)))
	    (other (buffer-substring (match-beginning 4) (match-end 4)))
	    temp)
	(if (> (setq temp (length filename)) longest-file)
	    (setq longest-file temp))
	(if (> (setq temp (length function)) longest-function)
	    (setq longest-function temp))
	(if (> (setq temp (length linenum)) longest-line)
	    (setq longest-line temp))
	(setq counter (1+ counter))))
    (cscope-file-vector-set (make-vector counter ""))
    (cscope-line-vector-set (make-vector counter 0))
    (setq return-value counter
	  counter 0)

    ;;
    ;; Go through buffer reformatting it to be pretty
    ;;
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)"
			      nil t)
      (let* ((full-filename
	      (buffer-substring (match-beginning 1) (match-end 1)))
	     (filename (file-name-nondirectory full-filename))
		       
	     (function (buffer-substring (match-beginning 2) (match-end 2)))
	     (linenum (buffer-substring (match-beginning 3) (match-end 3)))
	     (other (buffer-substring (match-beginning 4) (match-end 4)))
	     (filelen (length filename))
	     (funclen (length function))
	     (temp (format "%%3d %%s%%%ds%%s%%%ds%%%ds %%s"
			   (1+ (- longest-file filelen))
			   (1+ (- longest-function funclen))
			   longest-line)))
	(aset (cscope-file-vector-get) counter full-filename)
	(aset (cscope-line-vector-get) counter (string-to-number linenum))
	(replace-match (format temp (setq counter (1+ counter))
			       filename " " function " " linenum
			       other) t t)
	(put-text-property (save-excursion (beginning-of-line) (point))
			   (point)
			   'mouse-face 'highlight)

	;;
	;; ToDo: I want to make the five columns elastic.  When the
	;; window is too narrow, only a part of each column is
	;; displayed with the rest being invisible.  A help key would
	;; be added so if the user mouses over a field, the full name
	;; would appear as the tool tip.  As the window is made wider,
	;; the rightmost column would expand first.  Once it was fully
	;; expanded, then the other columns would grow until they were
	;; fully expanded.  When all are fully expanded, the excess
	;; white space would be left on the right margin as usual.
	;;
	;; I think if the number of results is greater than a preset
	;; limit, this would not be done or perhaps done "lazily".
	;;
	;; All this is possible and is actually more practical than it
	;; seems.  With long function names and file names, the
	;; results are wider than the current window and some type of
	;; compressing of the columns is needed.
	;;

	;;
	;; Preliminary version of striping the result for easier
	;; visibility but it needs to be done with a face that adapts
	;; to the background being used.  I'm also not sure I want to
	;; start down this path.
	;;
	;; (if (= (mod counter 2) 0)
	;;     (put-text-property (save-excursion (beginning-of-line) (point))
	;; 		       (point)
	;; 		       'face '(:background "white smoke")))
	))
    ;;
    ;; Go back through and wrap the long lines in a pretty fashion
    ;;
    ;; I no longer like this.  Too often, the left margin is wider
    ;; than the window and this creates an infinite loop.  I considered
    ;; various alternatives.  For example, it is possible to add a
    ;; property so that wrap automatically moves the a column but it
    ;; too suffers from an infinite loop of the wrap column is bigger
    ;; than the window size.  For now, lets just leave the display
    ;; with lines truncated and see how it goes.
    ;;
    ;; (goto-char (point-min))
    ;; (setq pat (concat "\\("
    ;; 		      (make-string (- (window-width) 2) ?.)
    ;; 		      "\\)\\(.+\\)"))
    ;; (while (re-search-forward pat nil t)
    ;;   (replace-match (concat
    ;; 		      (buffer-substring (match-beginning 1) (match-end 1))
    ;; 		      "\n"
    ;; 		      (make-string
    ;; 		       (+ longest-file longest-function longest-line 7) ? )
    ;; 		      (buffer-substring (match-beginning 2) (match-end 2)))
    ;; 		     t t)
    ;;   (put-text-property (match-beginning 1) (point) 'mouse-face 'highlight)
    ;;   (beginning-of-line))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    return-value))

(defun cscope-wait ()
  "Waits for the cscope process to finish and print the \">> \" prompt."
  (let ((process (cscope-process-get)))
    (with-current-buffer (cscope-out-buffer-get)
      (while (and (or (eq (process-status process) 'run)
		      (eq (process-status process) 'signal))
		  (progn
		    (goto-char (point-max))
		    (beginning-of-line)
		    (not (looking-at ">> "))))
	(accept-process-output process 1)))))

(defun cscope-send-string ( string )
  "Sends STRING to the cscope process.
Using `with-current-buffer' set to `cscope-out-buffer', STRING is sent
to `cscope-process'.  Then `cscope-wait' and `cscope-format' are
called."
  (with-current-buffer (cscope-get-or-create-out-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (send-string (get-buffer-process (current-buffer)) string)
    (cscope-wait)
    (cscope-format)
    (setq buffer-read-only t)))

(defun cscope-send-and-select ( string )
  "Calls `cscope-send-string' with STRING and then displays what the
user wants to see"
  (cscope-send-string string)
  (cscope-push-mark)
  (if (and (= (length (cscope-file-vector-get)) 1) cscope-auto-go)
      (progn
	(set-buffer (cscope-out-buffer-get))
	(cscope-goto-from-list nil))
    (pop-to-buffer (cscope-out-buffer-get))))

(defun cscope-mouse-no-op ( click )
  "Function which does nothing"
  (interactive "e"))

(defun cscope-get-line-number ()
  "Returns the \"line number\" out of a cscope output buffer"
  (end-of-line)
  (re-search-backward "^[ 0-9][ 0-9][0-9]+")
  (1- (string-to-number (buffer-substring (match-beginning 0) (match-end 0)))))

(defun cscope-view-from-list ()
  "Point is in a buffer pointing to a line produced by
cscope-list-line. This routine plops into the file at the appropriate
spot"
  (interactive)
  (let* ((num (cscope-get-line-number))
	 (fname (expand-file-name (aref (cscope-file-vector-get) num)))
	 (lnum (aref (cscope-line-vector-get) num)))
    (find-file fname)
    (goto-char (point-min))
    (forward-line (1- lnum))
    (view-buffer (current-buffer))))

(defun cscope-goto-from-list ( arg )
  "Point is in a buffer pointing to a line produced by
cscope-list-line. This routine plops into the file at the appropriate
spot"
  (interactive "P")
  (let* ((num (cscope-get-line-number))
	 (fname (expand-file-name (aref (cscope-file-vector-get) num)))
	 (lnum (aref (cscope-line-vector-get) num)))
    (if arg
	(find-file-other-window fname)
      (find-file fname))
    (goto-char (point-min))
    (forward-line (1- lnum))))

(defun cscope-mouse-goto-from-list-other-window (click)
  "Move to where the mouse is and then process the line"
  (interactive "@e")
  (goto-char (posn-point (event-start click)))
  (cscope-goto-from-list t))

(defun cscope-mouse-view-from-list ( click )
  "Move to where the mouse is and then process the line"
  (interactive "@e")
  (goto-char (posn-point (event-start click)))
  (cscope-view-from-list))

(defun cscope-push-mark ()
  "Pushes a mark on to the `cscope-mark-ring'"
  (cscope-mark-nth-set -1)
  (let ((this-mark (set-marker (make-marker) (point) (current-buffer))))
    (cscope-mark-ring-set (cons this-mark (cscope-mark-ring-get))))
  (when (> (length (cscope-mark-ring-get)) cscope-mark-ring-max)
    (move-marker (car (nthcdr cscope-mark-ring-max (cscope-mark-ring-get))) nil)
    (setcdr (nthcdr (1- cscope-mark-ring-max) (cscope-mark-ring-get)) nil)))

(defun cscope-start-new ( cscope options dir database )
  "Start a new cscope session using the executable specified by CSCOPE
with OPTIONS.  DIR specifies the base directory for the source.
  DATABASE specifies the path to the cscope database (e.g. cscope.out)"
  (interactive (concat "fPath to cscope executable: \n" 
		       "sOptions to pass (usually -d -q -l): \n"
		       "DBase Directory for cscope DB: \n"
		       "fPath to cscope db: "))
  (cscope-init-process cscope options dir database))

(defun cscope-goto-mark ( mark )
  "Displays the buffer and position specified by MARK.  If the
position is before `point-min' (i.e. the buffer has been
narrorwed), then point is put at `point-min'.  Likewise if
position points past `point-max', the point is put at
`point-max'."
  (let ((buffer (marker-buffer mark))
	(position (marker-position mark)))
    (switch-to-buffer buffer)
    (goto-char (or (and (< position (point-min))
			(point-min))
		   (and (> position (point-max))
			(point-max))
		   position))))

(defun cscope-sanitize-mark-ring ()
  "Removes markers from `cscope-mark-ring' whose buffers have been
deleted. Called from `cscope-previous-mark' and `cscope-next-mark'."
  (let ((temp (cscope-mark-ring-get))
	(head nil)
	(last nil)
	marker)
    (while temp
      (if (marker-buffer (setq marker (car temp)))
	  (progn
	    (setq head (or head temp))
	    (setq temp (cdr (setq last temp))))
	(set-marker marker nil)		;probably not needed
	(if last
	    (setcdr last (setq temp (cdr temp)))
	  (setq temp (cdr temp)))))
    (cscope-mark-ring-set head)))

;; I'd rather call these cscope-back-mark and cscope-formward-mark and
;; bind them to b and f but both f and F are already bound.
(defun cscope-previous-mark ()
  "On the first call, goes to the most recent marker in the
`cscope-mark-ring'.  This is the buffer and position when the
last cscope command such as `cscope-find-references' was
executed.  Subsequent calls move further back in the history.
And error is raised if already past the end of the list.
`cscope-next-mark' moves forward in the history."
  (interactive)
  (cscope-sanitize-mark-ring)
  (let ((the-mark (nth (cscope-mark-nth-set (1+ (cscope-mark-nth-get)))
		       (cscope-mark-ring-get))))
    (if (and (null the-mark)
	     (cscope-mark-nth-set (length (cscope-mark-ring-get))))
	(error "Past end of `cscope-mark-ring'")
      
      (cscope-goto-mark the-mark))))

(defun cscope-next-mark ()
  "Moves forward (more recent) in `cscope-mark-ring' and displays the
buffer and position specified.  `cscope-previous-mark' moves backward
further in the past.  An error is raised if already at the
front of the list."
  (interactive)
  (cscope-sanitize-mark-ring)
  (if (= (cscope-mark-nth-get) 0)
      (error "Already at the front of `cscope-mark-ring'")
    (cscope-goto-mark (nth (cscope-mark-nth-set (1- (cscope-mark-nth-get)))
			   (cscope-mark-ring-get)))))
  
;; ToDo: I want to figure out how to use the mouse to pick the symbol
;; to do the cscope search on.  By "figure out", I mean figure out
;; which mouse events to hook in to which the users will find the most
;; convenient.
(defmacro cscope-define-function ( name symbol doc str prompt )
  "Define three functions for a particular CScope search.
The nine search facilities that CScope provides need to be called
from a key sequence, a mouse event, or from a menu.  This macro allows
a single definition to provide the three different functions.

NAME is the base name for the functions to define.  \"cscope-\"
will be prepended to NAME to provide the kbd event driven
version, \"cscope-mouse-\" for the mouse event driven version,
and \"cscope-menu-\" for the menu driven version.

SYMBOL is the type of the single argument that the kbd driven version
accepts.

DOC is the doc string without any ending period for the kbd
driven function.  This will be modified slightly for the other
two versions.

STR is the digit string, e.g. \"0\" to prepend to what is sent to the
cscope process.

PROMPT is the prompt that the kbd event version uses to prompt for the
string.

The mouse driven version will place point at the start of the mouse
event and then search for the symbol found under the point.  The menu
driven version simply search for the symbol found under the current
location of the point."
  (declare (indent defun))
  (let ((kbd-name (intern (format "cscope-%s" name)))
	(kbd-doc (concat doc "."))
	(mouse-name (intern (format "cscope-mouse-%s" name)))
	(mouse-doc (concat doc " that mouse is pointing at."))
	(menu-name (intern (format "cscope-menu-%s" name)))
	(menu-doc (concat doc " that cursor is currently on.")))
    `(progn
       (defun ,kbd-name ( ,symbol )
	 ,kbd-doc
	 (interactive (get-symbol-interactively ,prompt (cscope-history-sym)))
	 (cscope-send-and-select (concat ,str ,symbol "\n")))
       (defun ,mouse-name ( click )
	 ,mouse-doc
	 (interactive "@e")
	 (goto-char (posn-point (event-start click)))
	 (cscope-send-and-select (concat ,str (get-symbol-non-interactively) "\n")))
       (defun ,menu-name ( )
	 ,menu-doc
	 (interactive)
	 (cscope-send-and-select (concat ,str (get-symbol-non-interactively) "\n"))))))

(cscope-define-function
  find-references symbol
  "Find references to the SYMBOL"
  "0"
  "Find references to symbol: ")

(cscope-define-function
  find-declarations symbol
  "Find declarations for the SYMBOL"
  "1"
  "Find declarations of symbol: ")

(cscope-define-function
  find-functions-called symbol
  "Find functions called by SYMBOL"
  "2"
  "Find calls from: ")

(cscope-define-function
  find-calling-functions symbol
  "Find calls to SYMBOL"
  "3"
  "Find calls to: ")

(cscope-define-function
  find-string string
  "Find STRING"
  "4"
  "Find string: ")

(cscope-define-function
  find-pattern pattern
  "Find regular expression PATTERN"
  "6"
  "Find pattern: ")

(cscope-define-function
  find-file pattern
  "Find files matching PATTERN"
  "7"
  "Find file (regexp): ")

(cscope-define-function
  find-file-include pattern
  "Find files which includes files matching PATTERN"
  "8"
  "Find files which include (regexp): ")

(cscope-define-function
  find-assignment symbol
  "Find assignments to SYMBOL"
  "9"
  "Find assignments to symbol: ")

(add-hook 'c-mode-hook 'cscope-mode)
(add-hook 'c++-mode-hook 'cscope-mode)

(provide 'cscope)

;;; end of cscope.el
