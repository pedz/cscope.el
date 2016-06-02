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
    (define-key parent cscope-key-command-prefix child)
    (define-key child (kbd "F") 'cscope-find-file)
    (define-key child (kbd "c") 'cscope-find-calling-functions)
    (define-key child (kbd "f") 'cscope-find-declarations)
    (define-key child (kbd "i") 'cscope-find-file-include)
    (define-key child (kbd "s") 'cscope-find-references)
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
  (message "cscope-get-or-create-out-buffer")
  (if (cscope-needs-to-be-started)
      (let ((var cscope-dir-patterns)
	    temp)
	(message "cscope-get-or-create-out-buffer1")
	(while (not (or (null var)
			(string-match (nth 0 (nth 0 var)) default-directory)))
	  (setq var (cdr var)))
	(if var
	    (progn
	      (setq temp (car var))
	      (message "cscope-get-or-create-out-buffer2 %s %s %s %s"
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
    ;; (message "cscope-process-get is process id %d but it should be %d"
    ;; 	     (process-id (cscope-process-get)) (process-id (get-buffer-process (current-buffer))))
    (send-string (get-buffer-process (current-buffer)) string)
    (cscope-wait)
    (cscope-format)
    (setq buffer-read-only t)))

(defun cscope-send-and-select ( string )
  "Calls `cscope-send-string' with STRING and then displays what the
user wants to see"
  (cscope-send-string string)
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
	 (interactive (get-symbol-interactively ,prompt))
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

(defun cscope-start-new ( cscope options dir database )
  (interactive "fPath to cscope executable: \nsOptions to pass (usually -d -q -l): \nDBase Directory for cscope DB: \nfPath to cscope db: ")
  (message "cscope is %s" cscope)
  (message "options are %s" options)
  (message "dir is %s" dir)
  (message "database is %s" database)
  (cscope-init-process cscope options dir database))

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

;;; END END

;(global-set-key [mouse-3] 'x-c-mode-cscope-func)
;(global-set-key [double-mouse-3] 'x-c-mode-cscope-sym)

;;;;;; (funcall (lookup-key cscope-c-minor-mode-menu (vector (car (x-popup-menu t cscope-c-minor-mode-menu)))))

;;;;
;;;;
;;;;  Original Old code from here down
;;;;
;;;;

;; ;; This is a set of routines which uses the output of cscope to do
;; ;; basically the same things as cscope except you don't have to leave
;; ;; emacs (what a concept!)
;; ;;
;; ;; Recommended use
;; ;;
;; ;; Setup all the cscope goodies
;; ;;(autoload 'cscope-find-goodies "cscope"
;; ;;	  "Creates a cscope buffer with lines that match STRING"
;; ;;	  t)
;; ;;(autoload 'cscope-find-func "cscope"
;; ;;	  "Find FUNCTION declaration (or a #define) using the cscope stuff"
;; ;;	    t)
;; ;;(autoload 'cscope-find-symbol "cscope"
;; ;;	  "Find all references to SYMBOL"
;; ;;	  t)
;; ;;(autoload 'cscope-find-func-call "cscope"
;; ;;	  "Find all calls to FUNCTION"
;; ;;	  t)
;; ;;(autoload 'cscope-find-file "cscope"
;; ;;	  "Finds files matching PATTERN"
;; ;;	  t)
;; ;;(autoload 'cscope-find-file-include "cscope"
;; ;;	  "Finds files which include the file matching PATTERN"
;; ;;	  t)
;; ;;
;; ;; bind the above functions to your favorite key sequence.
;; ;;

;; ;; Remove the old version 3 build stuff


;; (defvar cscope-start-time nil
;;   "Last modification time of cscope.out file when cscope was started")
;; (make-variable-buffer-local 'cscope-start-time)

;; (defvar cscope-program-name "cscope-front"
;;   "*Name of program to run for cscope stuff")

;; (defvar cscope-auto-go t
;;   "*When only 1 cscope entry is found, emacs automatically selects that
;; entry")

;; (defvar cscope-process nil
;;   "Holds the process object for 'cscope'")
;; (make-variable-buffer-inherited 'cscope-process)

;; (defvar cscope-file-vector nil
;;   "Holds the full path names for the files listed in the cscope output")
;; (make-variable-buffer-local 'cscope-file-vector)

;; (defvar cscope-line-vector nil
;;   "Holds the line numbers for the lines listed in the cscope output")
;; (make-variable-buffer-local 'cscope-line-vector)

;; (defvar cscope-mode-map nil "")

;; (if cscope-mode-map
;;     ()
;;   (setq cscope-mode-map (make-keymap))
;;   (define-key cscope-mode-map "n" 'next-line)
;;   (define-key cscope-mode-map " " 'next-line)
;;   (define-key cscope-mode-map "p" 'previous-line)
;;   (define-key cscope-mode-map "\177" 'previous-line)
;;   (define-key cscope-mode-map "v" 'cscope-view-from-list)
;;   (define-key cscope-mode-map "q" 'delete-window)
;;   (define-key cscope-mode-map "e" 'cscope-goto-from-list)
;;   (define-key cscope-mode-map [mouse-1] 'x-cscope-goto-from-list-other-window)
;;   (define-key cscope-mode-map [drag-mouse-1] 'x-cscope-no-op)
;;   (define-key cscope-mode-map [down-mouse-1] 'x-cscope-no-op)
;;   (define-key cscope-mode-map [mouse-2] 'x-cscope-goto-from-list)
;;   (define-key cscope-mode-map [drag-mouse-2] 'x-cscope-no-op)
;;   (define-key cscope-mode-map [down-mouse-2] 'x-cscope-no-op)
;;   (define-key cscope-mode-map [mouse-3] 'x-cscope-view-from-list)
;;   (define-key cscope-mode-map [drag-mouse-3] 'x-cscope-no-op)
;;   (define-key cscope-mode-map [down-mouse-3] 'x-cscope-no-op)
;;   )

;; ;;;###autoload
;; (defun cscope-mode ()
;;   "Major mode used to look at the cscope output stuff\n
;; Type:
;;   n to go to the next line
;;   Space to go to the next line
;;   p to go to the previous line
;;   Rubout to go to the previous line
;;   v to view the file and line
;;   q to delete the cscope window
;;   e to edit the file at the line
;; "
;;   (use-local-map cscope-mode-map)
;;   (setq truncate-lines t)
;;   (setq buffer-read-only t)
;;   (setq major-mode 'cscope-mode)
;;   (setq mode-name "C-Scope"))

;; ;;;###autoload
;; (defun cscope-find-symbol ( string )
;;   "Find all references to the SYMBOL."
;;   (interactive (get-symbol-interactively "Find symbol: "))
;;   (cscope-find-goodies (concat "0" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-func ( string )
;;   "Find a function declaration (or a #define) using the cscope stuff"
;;   (interactive (get-symbol-interactively "Find function: "))
;;   (cscope-find-goodies (concat "1" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-functions-called ( string )
;;   "Find all functions called by this function"
;;   (interactive (get-symbol-interactively "Find calls from: "))
;;   (cscope-find-goodies (concat "2" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-func-call ( string )
;;   "Find all calls to the function"
;;   (interactive (get-symbol-interactively "Find calls to: "))
;;   (cscope-find-goodies (concat "3" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-string ( string )
;;   "Find string using cscope stuff"
;;   (interactive (get-symbol-interactively "Find string: "))
;;   (cscope-find-goodies (concat "4" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-pattern ( string )
;;   "Finds egrep patter in cscope stuff"
;;   (interactive (get-symbol-interactively "Pattern: "))
;;   (cscope-find-goodies (concat "6" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-file ( string )
;;   "Finds files matching PATTERN"
;;   (interactive (get-symbol-interactively "Find file: "))
;;   (cscope-find-goodies (concat "7" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-file-include ( string )
;;   "Find the files which include the file matching PATTERN"
;;   (interactive (get-symbol-interactively "Find files which include: "))
;;   (cscope-find-goodies (concat "8" string "\n")))

;; ;;;###autoload
;; (defun cscope-find-assignment ( string )
;;   "Find assignments to variables and fields matching PATTERN"
;;   (interactive (get-symbol-interactively "Find assignments to symbol: "))
;;   (cscope-find-goodies (concat "9" string "\n")))

;; ;;;###autoload
;; (defun cscope-get-line-number ()
;;   "Returns the \"line number\" out of a cscope output buffer"
;;   (end-of-line)
;;   (re-search-backward "^[ 0-9][ 0-9][0-9]")
;;   (1- (string-to-number (buffer-substring (match-beginning 0) (match-end 0)))))

;; ;;;###autoload
;; (defun cscope-goto-from-list ( arg )
;;   "Point is in a buffer pointing to a line produced by
;; cscope-list-line. This routine plops into the file at the appropriate
;; spot"
;;   (interactive "P")
;;   (let* ((num (cscope-get-line-number))
;; 	 (fname (expand-file-name (aref cscope-file-vector num)))
;; 	 (lnum (aref cscope-line-vector num)))
;;     (if arg
;; 	(find-file-other-window fname)
;;       (find-file fname))
;;     (message (buffer-name))
;;     (goto-char (point-min))
;;     (forward-line (1- lnum))))

;; ;;;###autoload
;; (defun cscope-view-from-list ()
;;   "Point is in a buffer pointing to a line produced by
;; cscope-list-line. This routine plops into the file at the appropriate
;; spot"
;;   (interactive)
;;   (let* ((num (cscope-get-line-number))
;; 	 (fname (expand-file-name (aref cscope-file-vector num)))
;; 	 (lnum (aref cscope-line-vector num)))
;;     (find-file fname)
;;     (goto-char (point-min))
;;     (forward-line (1- lnum))
;;     (view-buffer (current-buffer))))

;; ;;;
;; ;;; We are very carefull to reference only the cscope-out-buffer
;; ;;; variable from a normal C file.  We then switch to that buffer and
;; ;;; use its values for cscope-process and any other inherited
;; ;;; variables.  If cscope-out-buffer is nil or if the buffer does not
;; ;;; exist, we assume we need to recrank a cscope for this file.
;; ;;; 

;; (defvar cscope-clone-dir nil
;;   "Directory for the top of the clone tree used by cscope")


;; ;;;###autoload
;; (defun cscope-520-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix520"
;; 			  "/520_SERVICE" ))

;; ;;;###autoload
;; (defun cscope-52B-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52B"
;; 			  "/52B_SERVICE" ))

;; ;;;###autoload
;; (defun cscope-52F-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52F"
;; 			  "/52F_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-52H-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52H"
;; 			  "/52H_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-52I-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52I"
;; 			  "/52I_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-52L-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52L"
;; 			  "/52L_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-52M-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52M"
;; 			  "/52M_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-52Q-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52Q"
;; 			  "/52Q_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-52S-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix52S"
;; 			  "/52S_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-530-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix530"
;; 			  "/530_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53A-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53A"
;; 			  "/53A_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53D-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53D"
;; 			  "/53D_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53E-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53E"
;; 			  "/53E_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53H-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53H"
;; 			  "/53H_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53J-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53J"
;; 			  "/53J_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53L-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53L"
;; 			  "/53L_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53N-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53N"
;; 			  "/53N_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53Q-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53Q"
;; 			  "/53Q_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53S-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53S"
;; 			  "/53S_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53V-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53V"
;; 			  "/53V_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-53X-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix53X"
;; 			  "/53X_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-610-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix610"
;; 			  "/610_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61B-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61B"
;; 			  "/61B_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61D-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61D"
;; 			  "/61D_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61F-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61F"
;; 			  "/61F_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61H-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61H"
;; 			  "/61H_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61J-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61J"
;; 			  "/61J_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61L-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61L"
;; 			  "/61L_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61N-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61N"
;; 			  "/61N_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61Q-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61Q"
;; 			  "/61Q_COMPLETE" ))


;; ;;;###autoload
;; (defun cscope-61S-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61S"
;;                           "/61S_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-61V-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61V"
;; 			  "/61V_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61X-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61X"
;; 			  "/61X_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-61Y-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix61Y"
;; 			  "/61Y_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-710-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix710"
;;                           "/710_SERVICE" ))
;; ;;;###autoload
;; (defun cscope-71B-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71B"
;;                           "/71B_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-71D-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71D"
;;                           "/71D_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-71F-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71F"
;;                           "/71F_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-71H-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71H"
;;                           "/71H_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-71J-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71J"
;;                           "/71J_COMPLETE" ))
;; ;;;###autoload
;; (defun cscope-71L-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/aix71L"
;;                           "/71L_COMPLETE" ))

;; ;;;###autoload
;; (defun cscope-ipf-process ( )
;;   (interactive)
;;   (cscope-bananas-are-fun "/gsa/ausgsa/projects/a/aix/isr/53ipfl53H"
;; 			  "/53ipfl53H_latest" ))

;; ;;;###autoload
;; (defun cscope-bananas-are-fun ( base build )
;;   (let ((new-buf (get-buffer-create "$&$&$&$"))
;; 	(data-dir (concat base "/cscope"))
;; 	(base-dir (concat base build ))
;; 	(count 0)
;; 	l out reply link)
;;     (set-buffer new-buf)
;;     (setq cscope-out-buffer nil)
;;     (setq default-directory base-dir)
;;     (setq l (directory-files data-dir nil ".*\.db$"))
;;     (while l
;;       (setq link (concat data-dir "/" (car l)))
;;       (while (stringp (setq link (car (file-attributes link))))
;;         nil)
;;       (if (null link)
;;           (setq out (append out (list (cons (car l) (setq count (1+ count)))))))
;;       (setq l (cdr l)))
;;     (setq reply (completing-read "Cscope database: " out nil t "mono.db"))
;;     (cscope-init-process (concat data-dir "/" reply) reply)
;;     (kill-buffer new-buf)))

;; ;;;###autoload
;; (defun cscope-new-process ( )
;;   (interactive)
;;   (setq cscope-out-buffer nil)
;;   (cscope-init-process "cscope.out" ""))

;; ;;;###autoload
;; (defun cscope-init-process ( cscope-out dbname )
;;   (condition-case dummy
;;       (set-buffer cscope-out-buffer)
;;     (error 
;;      (let* ((old-buf (current-buffer))
;; 	    (dir-temp (expand-file-name
;; 		       (read-file-name "Dir with cscope file "
;; 				       default-directory default-directory t)))
;; 	    (clone-temp dir-temp)
;; 	    (full-name (concat dir-temp "CSCOPE" dbname))
;; 	    (buf-name (concat "cscope:" dbname " " (file-name-nondirectory
;; 						    (directory-file-name
;; 						     dir-temp)))))
;; 	;
;; 	; We want to find a cscope buffer if we already have a cscope
;; 	; started in this directory.  We set buffer-file-name equal to
;; 	; the directory path with "CSCOPE" appended and use that to
;; 	; search with.  We eventuall set the buffer name to a shorter
;; 	; and nicer looking "cscope: dir" where dir is the last
;; 	; directory in the path
;; 	;
;; 	(if (setq cscope-out-buffer (get-file-buffer full-name))
;; 	    (set-buffer cscope-out-buffer)
;; 	  (set-buffer
;; 	   (setq cscope-out-buffer (create-file-buffer buf-name)))
;; 	  (setq buffer-file-name full-name
;; 		cscope-clone-dir clone-temp
;; 		default-directory dir-temp)
;; 	  (inherit-from-buffer old-buf)
;; 	  (setq cscope-process nil)))))
;;   (pop-to-buffer cscope-out-buffer)
;;   (let ((mod-time (nth 5 (file-attributes "cscope.out")))
;; 	(dead-proc (or (null cscope-process)
;; 		       (null (eq (process-status cscope-process) 'run)))))
;;     (if (and (not dead-proc)
;; 	     (not (equal cscope-start-time mod-time)))
;; 	(progn
;; 	  (kill-process cscope-process)
;; 	  (setq dead-proc t)))
;;     (if dead-proc
;; 	(progn
;; 	  (if cscope-process
;; 	      (message "Restarting dead cscope-process...")
;; 	    (message "Starting new cscope process..."))
;; 	  (set-process-query-on-exit-flag
;; 	   (setq cscope-process (start-process "cscope" cscope-out-buffer
;; 					       cscope-program-name
;; 					       ""
;; 					       cscope-clone-dir
;; 					       "-f" cscope-out)) nil)
;; 	  (cscope-wait)
;; 	  (setq cscope-start-time mod-time))))
;;   (setq buffer-read-only nil)
;;   (erase-buffer))

;; ;;;###autoload
;; (defun cscope-wait ()
;;   "Waits for the cscope process to finish"
;;   (message "Waiting for cscope...")
;;   (while (and (eq (process-status cscope-process) 'run)
;; 	      (progn
;; 		(goto-char (point-max))
;; 		(beginning-of-line)
;; 		(not (looking-at ">> "))))
;;     (accept-process-output cscope-process)))

;; ;;;###autoload
;; (defun cscope-format ()
;;   "Formats the output of cscope to be pretty"
;;   (let ((longest-file 0)
;; 	(longest-function 0)
;; 	(longest-line 0)
;; 	(counter 0)
;; 	pat return-value)
;;     (goto-char (point-max))
;;     (beginning-of-line)
;;     (delete-region (point)
;; 		   (save-excursion
;; 		     (forward-line 1)
;; 		     (point)))
;;     (goto-char (point-min))
;;     (delete-region (point)
;; 		   (save-excursion
;; 		     (forward-line 1)
;; 		     (point)))
;;     ;;
;;     ;; Go through buffer finding the longest filename, function name,
;;     ;; and line number.
;;     ;; 
;;     (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)"
;; 			      nil t)
;;       (let ((filename (file-name-nondirectory
;; 		       (buffer-substring (match-beginning 1) (match-end 1))))
;; 	    (function (buffer-substring (match-beginning 2) (match-end 2)))
;; 	    (linenum (buffer-substring (match-beginning 3) (match-end 3)))
;; 	    (other (buffer-substring (match-beginning 4) (match-end 4)))
;; 	    temp)
;; 	(if (> (setq temp (length filename)) longest-file)
;; 	    (setq longest-file temp))
;; 	(if (> (setq temp (length function)) longest-function)
;; 	    (setq longest-function temp))
;; 	(if (> (setq temp (length linenum)) longest-line)
;; 	    (setq longest-line temp))
;; 	(setq counter (1+ counter))))
;;     (setq cscope-file-vector (make-vector counter "")
;; 	  cscope-line-vector (make-vector counter 0)
;; 	  return-value counter
;; 	  counter 0)

;;     ;;
;;     ;; Go through buffer reformatting it to be pretty
;;     ;;
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\(.*\\)"
;; 			      nil t)
;;       (let* ((full-filename
;; 	      (buffer-substring (match-beginning 1) (match-end 1)))
;; 	     (filename (file-name-nondirectory full-filename))
		       
;; 	     (function (buffer-substring (match-beginning 2) (match-end 2)))
;; 	     (linenum (buffer-substring (match-beginning 3) (match-end 3)))
;; 	     (other (buffer-substring (match-beginning 4) (match-end 4)))
;; 	     (filelen (length filename))
;; 	     (funclen (length function))
;; 	     (temp (format "%%3d %%s%%%ds%%s%%%ds%%%ds %%s"
;; 			   (1+ (- longest-file filelen))
;; 			   (1+ (- longest-function funclen))
;; 			   longest-line)))
;; 	(aset cscope-file-vector counter full-filename)
;; 	(aset cscope-line-vector counter (string-to-number linenum))
;; 	(replace-match (format temp (setq counter (1+ counter))
;; 			       filename " " function " " linenum
;; 			       other) t t)
;; 	(put-text-property (save-excursion (beginning-of-line) (point))
;; 			   (point)
;; 			   'mouse-face 'highlight)))
;;     ;;
;;     ;; Go back through and wrap the long lines in a pretty fashion
;;     ;;
;;     (goto-char (point-min))
;;     (setq pat (concat "\\("
;; 		      (make-string (- (window-width) 2) ?.)
;; 		      "\\)\\(.+\\)"))
;;     (while (re-search-forward pat nil t)
;;       (replace-match (concat
;; 		      (buffer-substring (match-beginning 1) (match-end 1))
;; 		      "\n"
;; 		      (make-string
;; 		       (+ longest-file longest-function longest-line 7) ? )
;; 		      (buffer-substring (match-beginning 2) (match-end 2)))
;; 		     t t)
;;       (put-text-property (match-beginning 1) (point) 'mouse-face 'highlight)
;;       (beginning-of-line))
;;     (goto-char (point-min))
;;     (not-modified)
;;     return-value))

;; (defun kill-cscope-buffers ( buf )
;;   "Given a cscope buffer, kills all the buffers that have it as their
;;   cscope-out-buffer."
;;   (interactive "bBuffer: ")
;;   (let* ((v (buffer-local-value 'cscope-out-buffer (get-buffer buf))))
;;     (mapcar (lambda ( x )
;; 	      (if (eq v (buffer-local-value 'cscope-out-buffer x))
;; 		  (kill-buffer x)))
;; 	    (buffer-list))))

;; ;;;###autoload
;; (defun cscope-find-goodies ( string )
;;   "Calls the cscope program and sends it STRING, plops into the buffer
;; and puts the buffer into cscope-mode"
;;   (interactive "sString: \nP")
;;   (cscope-init-process "cscope.out" "")
;;   (send-string cscope-process string)
;;   (cscope-wait)
;;   (if (and (= (cscope-format) 1) cscope-auto-go)
;;       (cscope-goto-from-list nil)
;;     (pop-to-buffer cscope-out-buffer)
;;     (cscope-mode)))

;; ;;;###autoload
;; (defun x-cscope-goto-from-list ( click )
;;   "Move to where the mouse is and then process the line"
;;   (interactive "@e")
;;   (goto-char (posn-point (event-start click)))
;;   (cscope-goto-from-list nil))

;; ;;;###autoload
;; (defun x-cscope-goto-from-list-other-window (click)
;;   "Move to where the mouse is and then process the line"
;;   (interactive "@e")
;;   (goto-char (posn-point (event-start click)))
;;   (cscope-goto-from-list t))

;; ;;;###autoload
;; (defun x-cscope-view-from-list ( click )
;;   "Move to where the mouse is and then process the line"
;;   (interactive "@e")
;;   (goto-char (posn-point (event-start click)))
;;   (cscope-view-from-list))

;; ;;
;; ;; Neat features for c-mode so that we can cscope from the mouse
;; ;;
;; ;;;###autoload
;; (defun x-c-mode-cscope-sym ( click )
;;   "Find symbol via cscope that mouse is pointing at"
;;   (interactive "@e")
;;   (goto-char (posn-point (event-start click)))
;;   (cscope-find-goodies (concat "0" (get-symbol-non-interactively) "\n")))

;; ;;;###autoload
;; (defun x-c-mode-cscope-func ( click )
;;   "Find function via cscope that mouse is pointing at"
;;   (interactive "@e")
;;   (goto-char (posn-point (event-start click)))
;;   (cscope-find-goodies (concat "1" (get-symbol-non-interactively) "\n")))

;; ;;;###autoload
;; (defun x-cscope-no-op ( click )
;;   "Function which does nothing"
;;   (interactive "e"))

;; ;;; From backing list
;; ;;; \(^.*\)/aix\(...\)/\(5200\|5300\|6100\|7100\)\(-[0-9][0-9]\)\(-[0-9][0-9]\)?\(Gold\|_SP\)
;; ;;; (defalias 'cscope-\3\4\5 'cscope-\2-process)

;; (defalias 'cscope-5200-06 'cscope-52I-process)
;; (defalias 'cscope-5200-07 'cscope-52L-process)
;; (defalias 'cscope-5200-08 'cscope-52M-process)
;; (defalias 'cscope-5200-09-01 'cscope-52Q-process)
;; (defalias 'cscope-5200-09-02 'cscope-52Q-process)
;; (defalias 'cscope-5200-09-03 'cscope-52Q-process)
;; (defalias 'cscope-5200-09-04 'cscope-52Q-process)
;; (defalias 'cscope-5200-09-05 'cscope-52Q-process)
;; (defalias 'cscope-5200-09-06 'cscope-52Q-process)
;; (defalias 'cscope-5200-09 'cscope-52Q-process)
;; (defalias 'cscope-5200-10-01 'cscope-52S-process)
;; (defalias 'cscope-5200-10-02 'cscope-52S-process)
;; (defalias 'cscope-5200-10-03 'cscope-52S-process)
;; (defalias 'cscope-5200-10-04 'cscope-52S-process)
;; (defalias 'cscope-5200-10-05 'cscope-52S-process)
;; (defalias 'cscope-5200-10-06 'cscope-52S-process)
;; (defalias 'cscope-5200-10-07 'cscope-52S-process)
;; (defalias 'cscope-5200-10-08 'cscope-52S-process)
;; (defalias 'cscope-5200-10 'cscope-52S-process)
;; (defalias 'cscope-5300-02 'cscope-53A-process)
;; (defalias 'cscope-5300-03 'cscope-53D-process)
;; (defalias 'cscope-5300-04 'cscope-53E-process)
;; (defalias 'cscope-5300-05-01 'cscope-53H-process)
;; (defalias 'cscope-5300-05-02 'cscope-53H-process)
;; (defalias 'cscope-5300-05-03 'cscope-53H-process)
;; (defalias 'cscope-5300-05-04 'cscope-53H-process)
;; (defalias 'cscope-5300-05-05 'cscope-53H-process)
;; (defalias 'cscope-5300-05-06 'cscope-53H-process)
;; (defalias 'cscope-5300-05 'cscope-53H-process)
;; (defalias 'cscope-5300-06-01 'cscope-53J-process)
;; (defalias 'cscope-5300-06-02 'cscope-53J-process)
;; (defalias 'cscope-5300-06-03 'cscope-53J-process)
;; (defalias 'cscope-5300-06-04 'cscope-53J-process)
;; (defalias 'cscope-5300-06-05 'cscope-53J-process)
;; (defalias 'cscope-5300-06-06 'cscope-53J-process)
;; (defalias 'cscope-5300-06-07 'cscope-53J-process)
;; (defalias 'cscope-5300-06-08 'cscope-53J-process)
;; (defalias 'cscope-5300-06-09 'cscope-53J-process)
;; (defalias 'cscope-5300-06-10 'cscope-53J-process)
;; (defalias 'cscope-5300-06-11 'cscope-53J-process)
;; (defalias 'cscope-5300-06-12 'cscope-53J-process)
;; (defalias 'cscope-5300-06 'cscope-53J-process)
;; (defalias 'cscope-5300-07-01 'cscope-53L-process)
;; (defalias 'cscope-5300-07-02 'cscope-53L-process)
;; (defalias 'cscope-5300-07-03 'cscope-53L-process)
;; (defalias 'cscope-5300-07-04 'cscope-53L-process)
;; (defalias 'cscope-5300-07-05 'cscope-53L-process)
;; (defalias 'cscope-5300-07-06 'cscope-53L-process)
;; (defalias 'cscope-5300-07-07 'cscope-53L-process)
;; (defalias 'cscope-5300-07-08 'cscope-53L-process)
;; (defalias 'cscope-5300-07-09 'cscope-53L-process)
;; (defalias 'cscope-5300-07-10 'cscope-53L-process)
;; (defalias 'cscope-5300-07 'cscope-53L-process)
;; (defalias 'cscope-5300-08-01 'cscope-53N-process)
;; (defalias 'cscope-5300-08-02 'cscope-53N-process)
;; (defalias 'cscope-5300-08-03 'cscope-53N-process)
;; (defalias 'cscope-5300-08-04 'cscope-53N-process)
;; (defalias 'cscope-5300-08-05 'cscope-53N-process)
;; (defalias 'cscope-5300-08-06 'cscope-53N-process)
;; (defalias 'cscope-5300-08-07 'cscope-53N-process)
;; (defalias 'cscope-5300-08-08 'cscope-53N-process)
;; (defalias 'cscope-5300-08-09 'cscope-53N-process)
;; (defalias 'cscope-5300-08-10 'cscope-53N-process)
;; (defalias 'cscope-5300-08 'cscope-53N-process)
;; (defalias 'cscope-5300-09-01 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-02 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-03 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-04 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-05 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-06 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-07 'cscope-53Q-process)
;; (defalias 'cscope-5300-09-08 'cscope-53Q-process)
;; (defalias 'cscope-5300-09 'cscope-53Q-process)
;; (defalias 'cscope-5300-10-01 'cscope-53S-process)
;; (defalias 'cscope-5300-10-02 'cscope-53S-process)
;; (defalias 'cscope-5300-10-03 'cscope-53S-process)
;; (defalias 'cscope-5300-10-04 'cscope-53S-process)
;; (defalias 'cscope-5300-10-05 'cscope-53S-process)
;; (defalias 'cscope-5300-10-06 'cscope-53S-process)
;; (defalias 'cscope-5300-10-07 'cscope-53S-process)
;; (defalias 'cscope-5300-10 'cscope-53S-process)
;; (defalias 'cscope-5300-11-01 'cscope-53V-process)
;; (defalias 'cscope-5300-11-02 'cscope-53V-process)
;; (defalias 'cscope-5300-11-03 'cscope-53V-process)
;; (defalias 'cscope-5300-11-04 'cscope-53V-process)
;; (defalias 'cscope-5300-11-05 'cscope-53V-process)
;; (defalias 'cscope-5300-11-06 'cscope-53V-process)
;; (defalias 'cscope-5300-11-07 'cscope-53V-process)
;; (defalias 'cscope-5300-11-08 'cscope-53V-process)
;; (defalias 'cscope-5300-11 'cscope-53V-process)
;; (defalias 'cscope-5300-12-01 'cscope-53X-process)
;; (defalias 'cscope-5300-12-02 'cscope-53X-process)
;; (defalias 'cscope-5300-12-03 'cscope-53X-process)
;; (defalias 'cscope-5300-12-04 'cscope-53X-process)
;; (defalias 'cscope-5300-12-05 'cscope-53X-process)
;; (defalias 'cscope-5300-12-06 'cscope-53X-process)
;; (defalias 'cscope-5300-12-07 'cscope-53X-process)
;; (defalias 'cscope-5300-12-08 'cscope-53X-process)
;; (defalias 'cscope-5300-12 'cscope-53X-process)
;; (defalias 'cscope-6100-00-01 'cscope-540-process)
;; (defalias 'cscope-6100-00-02 'cscope-540-process)
;; (defalias 'cscope-6100-00-03 'cscope-540-process)
;; (defalias 'cscope-6100-00-04 'cscope-540-process)
;; (defalias 'cscope-6100-00-05 'cscope-540-process)
;; (defalias 'cscope-6100-00-06 'cscope-540-process)
;; (defalias 'cscope-6100-00-07 'cscope-540-process)
;; (defalias 'cscope-6100-00-08 'cscope-540-process)
;; (defalias 'cscope-6100-00-09 'cscope-540-process)
;; (defalias 'cscope-6100-00-10 'cscope-540-process)
;; (defalias 'cscope-6100-00-11 'cscope-540-process)
;; (defalias 'cscope-6100-00-01 'cscope-610-process)
;; (defalias 'cscope-6100-00-02 'cscope-610-process)
;; (defalias 'cscope-6100-00-03 'cscope-610-process)
;; (defalias 'cscope-6100-00-04 'cscope-610-process)
;; (defalias 'cscope-6100-00-05 'cscope-610-process)
;; (defalias 'cscope-6100-00-06 'cscope-610-process)
;; (defalias 'cscope-6100-00-07 'cscope-610-process)
;; (defalias 'cscope-6100-00-08 'cscope-610-process)
;; (defalias 'cscope-6100-00-09 'cscope-610-process)
;; (defalias 'cscope-6100-00-10 'cscope-610-process)
;; (defalias 'cscope-6100-00-11 'cscope-610-process)
;; (defalias 'cscope-6100-01-01 'cscope-61B-process)
;; (defalias 'cscope-6100-01-02 'cscope-61B-process)
;; (defalias 'cscope-6100-01-03 'cscope-61B-process)
;; (defalias 'cscope-6100-01-04 'cscope-61B-process)
;; (defalias 'cscope-6100-01-05 'cscope-61B-process)
;; (defalias 'cscope-6100-01-06 'cscope-61B-process)
;; (defalias 'cscope-6100-01-07 'cscope-61B-process)
;; (defalias 'cscope-6100-01-08 'cscope-61B-process)
;; (defalias 'cscope-6100-01-09 'cscope-61B-process)
;; (defalias 'cscope-6100-01 'cscope-61B-process)
;; (defalias 'cscope-6100-02-01 'cscope-61D-process)
;; (defalias 'cscope-6100-02-02 'cscope-61D-process)
;; (defalias 'cscope-6100-02-03 'cscope-61D-process)
;; (defalias 'cscope-6100-02-04 'cscope-61D-process)
;; (defalias 'cscope-6100-02-05 'cscope-61D-process)
;; (defalias 'cscope-6100-02-06 'cscope-61D-process)
;; (defalias 'cscope-6100-02-07 'cscope-61D-process)
;; (defalias 'cscope-6100-02-08 'cscope-61D-process)
;; (defalias 'cscope-6100-02-09 'cscope-61D-process)
;; (defalias 'cscope-6100-02-10 'cscope-61D-process)
;; (defalias 'cscope-6100-02 'cscope-61D-process)
;; (defalias 'cscope-6100-03-01 'cscope-61F-process)
;; (defalias 'cscope-6100-03-02 'cscope-61F-process)
;; (defalias 'cscope-6100-03-03 'cscope-61F-process)
;; (defalias 'cscope-6100-03-04 'cscope-61F-process)
;; (defalias 'cscope-6100-03-05 'cscope-61F-process)
;; (defalias 'cscope-6100-03-06 'cscope-61F-process)
;; (defalias 'cscope-6100-03-07 'cscope-61F-process)
;; (defalias 'cscope-6100-03-08 'cscope-61F-process)
;; (defalias 'cscope-6100-03-09 'cscope-61F-process)
;; (defalias 'cscope-6100-03-10 'cscope-61F-process)
;; (defalias 'cscope-6100-03 'cscope-61F-process)
;; (defalias 'cscope-6100-04-01 'cscope-61H-process)
;; (defalias 'cscope-6100-04-02 'cscope-61H-process)
;; (defalias 'cscope-6100-04-03 'cscope-61H-process)
;; (defalias 'cscope-6100-04-04 'cscope-61H-process)
;; (defalias 'cscope-6100-04-05 'cscope-61H-process)
;; (defalias 'cscope-6100-04-06 'cscope-61H-process)
;; (defalias 'cscope-6100-04-07 'cscope-61H-process)
;; (defalias 'cscope-6100-04-08 'cscope-61H-process)
;; (defalias 'cscope-6100-04-09 'cscope-61H-process)
;; (defalias 'cscope-6100-04-10 'cscope-61H-process)
;; (defalias 'cscope-6100-04-11 'cscope-61H-process)
;; (defalias 'cscope-6100-04 'cscope-61H-process)
;; (defalias 'cscope-6100-05-01 'cscope-61J-process)
;; (defalias 'cscope-6100-05-02 'cscope-61J-process)
;; (defalias 'cscope-6100-05-03 'cscope-61J-process)
;; (defalias 'cscope-6100-05-04 'cscope-61J-process)
;; (defalias 'cscope-6100-05-05 'cscope-61J-process)
;; (defalias 'cscope-6100-05-06 'cscope-61J-process)
;; (defalias 'cscope-6100-05-07 'cscope-61J-process)
;; (defalias 'cscope-6100-05-08 'cscope-61J-process)
;; (defalias 'cscope-6100-05-09 'cscope-61J-process)
;; (defalias 'cscope-6100-05 'cscope-61J-process)
;; (defalias 'cscope-6100-06-01 'cscope-61L-process)
;; (defalias 'cscope-6100-06-02 'cscope-61L-process)
;; (defalias 'cscope-6100-06-03 'cscope-61L-process)
;; (defalias 'cscope-6100-06-04 'cscope-61L-process)
;; (defalias 'cscope-6100-06 'cscope-61L-process)
;; (defalias 'cscope-6100-06-05 'cscope-61N-process)
;; (defalias 'cscope-6100-06-06 'cscope-61N-process)
;; (defalias 'cscope-6100-06-07 'cscope-61N-process)
;; (defalias 'cscope-6100-06-08 'cscope-61N-process)
;; (defalias 'cscope-6100-06-09 'cscope-61N-process)
;; (defalias 'cscope-6100-06-10 'cscope-61N-process)
;; (defalias 'cscope-6100-06-11 'cscope-61N-process)
;; (defalias 'cscope-6100-06-12 'cscope-61N-process)
;; (defalias 'cscope-6100-07-01 'cscope-61Q-process)
;; (defalias 'cscope-6100-07-02 'cscope-61Q-process)
;; (defalias 'cscope-6100-07-03 'cscope-61Q-process)
;; (defalias 'cscope-6100-07 'cscope-61Q-process)
;; (defalias 'cscope-6100-07-04 'cscope-61S-process)
;; (defalias 'cscope-6100-07-05 'cscope-61S-process)
;; (defalias 'cscope-6100-07-06 'cscope-61S-process)
;; (defalias 'cscope-6100-07-07 'cscope-61S-process)
;; (defalias 'cscope-6100-07-08 'cscope-61S-process)
;; (defalias 'cscope-6100-08-01 'cscope-61V-process)
;; (defalias 'cscope-6100-08 'cscope-61V-process)
;; (defalias 'cscope-6100-08-02 'cscope-61X-process)
;; (defalias 'cscope-6100-08-03 'cscope-61X-process)
;; (defalias 'cscope-7100-00-01 'cscope-710-process)
;; (defalias 'cscope-7100-00-02 'cscope-710-process)
;; (defalias 'cscope-7100-00-03 'cscope-71B-process)
;; (defalias 'cscope-7100-00-04 'cscope-71B-process)
;; (defalias 'cscope-7100-00-05 'cscope-71B-process)
;; (defalias 'cscope-7100-00-06 'cscope-71B-process)
;; (defalias 'cscope-7100-00-07 'cscope-71B-process)
;; (defalias 'cscope-7100-00-08 'cscope-71B-process)
;; (defalias 'cscope-7100-00-09 'cscope-71B-process)
;; (defalias 'cscope-7100-00-10 'cscope-71B-process)
;; (defalias 'cscope-7100-01-01 'cscope-71D-process)
;; (defalias 'cscope-7100-01-02 'cscope-71D-process)
;; (defalias 'cscope-7100-01-03 'cscope-71D-process)
;; (defalias 'cscope-7100-01 'cscope-71D-process)
;; (defalias 'cscope-7100-01-04 'cscope-71F-process)
;; (defalias 'cscope-7100-01-05 'cscope-71F-process)
;; (defalias 'cscope-7100-01-06 'cscope-71F-process)
;; (defalias 'cscope-7100-01-07 'cscope-71F-process)
;; (defalias 'cscope-7100-01-08 'cscope-71F-process)
;; (defalias 'cscope-7100-02-01 'cscope-71H-process)
;; (defalias 'cscope-7100-02 'cscope-71H-process)
;; (defalias 'cscope-7100-02-02 'cscope-71J-process)
;; (defalias 'cscope-7100-02-03 'cscope-71J-process)

(provide 'cscope)

;;; end of cscope.el
