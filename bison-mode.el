;;; bison-mode.el --- Major mode for editing bison, yacc and lex files.

;; Copyright (C) 1998 Eric Beuscher
;;
;; Author:   Eric Beuscher <beuscher@eecs.tulane.edu>
;; Created:  2 Feb 1998
;; Version:  0.2
;; Keywords: bison-mode, yacc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;;; I wrote this since I saw one mode for yacc files out there roaming the
;;;; world.     I was daunted by the fact the it was written in 1990, and Emacs
;;;; has evolved so much since then (this I assume based on its evolution since
;;;; i started using it).     So I figured if i wanted one, I should make it
;;;; myself.     Please excuse idiosyncrasies, as this was my first major mode
;;;; of this kind.     The indentation code may be a bit weird, I am not sure,
;;;; it was my first go at doing Emacs indentation, so I look at how other
;;;; modes did it, but then basically did what I thought was right

;;;; I hope this is useful to other hackers, and happy Bison/Yacc hacking
;;;; If you have ideas/suggestions/problems with this code, I can be reached at
;;;; beuscher@eecs.tulane.edu

;;;; Eric --- Sat Mar  7 1:40:20 CDT 1998

;;;; Bison Sections:
;;;; there are five sections to a bison file (if you include the area above the
;;;; C declarations section.     most everything in this file either does
;;;; actions based on which section you are deemed to be in, or based on an
;;;; assumption that the function will only be called from certain sections.
;;;; the function `bison--section' is the section parser

;;;; Indentation:
;;;; indentations are done based on the section of code you are in.    there is
;;;; a procedure `bison--within-braced-c-expression-p' that checks for being in
;;;; C code.    if you are within c-code, indentations should occur based on
;;;; how you have your C indentation set up.     i am pretty sure this is the
;;;; case.
;;;; there are four variables, which control bison indentation within either
;;;; the bison declarations section or the bison grammar section
;;;; `bison-rule-separator-column'
;;;; `bison-rule-separator-column'
;;;; `bison-decl-type-column'
;;;; `bison-decl-token-column'

;;;; flaw: indentation works on a per-line basis, unless within braced C sexp,
;;;; i should fix this someday
;;;; and to make matters worse, i never took out c-indent-region, so that is
;;;; still the state of the `indent-region-function' variable

;;;; Electricity:
;;;; by default, there are electric -colon, -pipe, -open-brace, -close-brace,
;;;; -semicolon, -percent, -less-than, -greater-than
;;;; the indentation caused by these work closely with the 4 indentation
;;;; variables mentioned above.
;;;; any of these can be turned off individually by setting the appropriate
;;;; `bison-electric-...' variable.     or all of them can be turned off by
;;;; setting `bison-electricity-off'

;;;; todo:  should make available a way to use C-electricity if in C sexps

;;; Code:

(require 'cc-mode)
(require 'seq)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.l\\'" . bison-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jison\\'" . jison-mode))

;;;  Symbols to designate various sections of Bison file.
;;
;; :init-section        Section before cdecl-section, if that section exists.
;; :c-decls-section     Section denoted by %{ and %} for c-declarations at the top of a bison file.
;; :bison-decls-section Bison declarations (before the rules section and not C declarations).
;; :rules-section       Section delimited by %%'s where productions and rules are enumerated.
;; :c-code-section      Section after the second %% where c-code can be placed.

;;;  Custom variables

;;;###autoload
(defgroup bison nil
  "Major mode for editing Bison and Yacc files."
  :prefix "bison-"
  :group 'languages)

;;;###autoload
(defcustom bison-mode-hook nil
    "*Hook called by `bison-mode'."
    :type  'hook
    :group 'bison)

;;;###autoload
(defcustom bison-rule-separator-column 8
  "*Column for rule and production separators \"|\" and \";\"."
  :type  'integer
  :group 'bison)

;;;###autoload
(defcustom bison-rule-enumeration-column 16
  "*Column  for beginning enumeration of a production's rules."
  :type  'integer
  :group 'bison)

;;;###autoload
(defcustom bison-decl-type-column 8
  "*Column  in which tokens' and states' types should be when declared."
  :type  'integer
  :group 'bison)

;;;###autoload
(defcustom bison-decl-token-column 24
  "*Column in which tokens and states are listed when declared, as with %token, %type, ..."
  :type  'integer
  :group 'bison)

;;;###autoload
(defcustom bison-electric-: t
  "Non-nil means use an electric colon."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-| t
  "Non-nil means use an electric pipe."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-{ t
  "Non-nil means use an electric open-brace."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-} t
  "Non-nil means use an electric close-brace."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-\; t
  "Non-nil means use an electric semicolon."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-% t
  "Non-nil means use an electric percent."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-lt t
  "Non-nil means use an electric less-than."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electric-gt t
  "Non-nil means use an electric greater-than."
  :type  'boolean
  :group 'bison)

;;;###autoload
(defcustom bison-electricity-off nil
  "*Disable all ‘bison-mode’ electric keys.

nil means that a bison-electric-* key will be on or off based on the individual
key's electric variable."
  :type  'boolean
  :group 'bison)

;;;  Lexical regexps

(defconst bison--word-constituent-re "\\(?:\\sw\\|_\\)")
(defconst bison--production-re  (concat "^" bison--word-constituent-re "+:"))
(defconst bison--action-re  (concat "\\s-+\\(" bison--word-constituent-re "+\\|/[*]\\s-*empty\\s-*[*]/\\|'.'\\|\"[^\"]+\"\\)"))

(defconst bison--c-section-opener "^%{"
  "Regular expression denoting start of C/C++ code sections.")
(defconst bison--c-section-closer "^%}"
  "Regular expression denoting end of C/C++ code sections.")
(defconst bison--rules-section-delimeter "^%%"
  "Regular expression denoting start and end of bison grammar rules section.")

(defconst bison--section-open-re
  (concat "\\(?:"
          "\\(" bison--c-section-opener "\\)\\|"
          "\\(" bison--rules-section-delimeter "\\)"
          "\\)"))

(defconst bison--sections-dfa
  `((:c-decls-section   ,bison--c-section-closer        :bison-decls-section)
    (:rules-section     ,bison--rules-section-delimeter :c-code-section)))

;;;  Keywords

(defconst bison--declarers '("%union" "%token" "%type"
			   "%left" "%right" "%nonassoc")
  "Commands that can declare a token or state type.")

(defconst bison--directives
  '("%code"
    "%debug" "%define" "%defines" "%destructor" "%dprec"
    "%error-verbose"
    "%file-prefix"
    "%glr-parser"
    "%initial-action"
    "%language" "%locations" "%lex-param"
    "%no-lines"
    "%output"
    "%param" "%parse-param" "%prec" "%precedence" "%pure-parser"
    "%require"
    "%skeleton" "%start"
    "%token-table")

  "Bison directives, except of those in ‘bison--declarers’.")

(defconst bison-font-lock-keywords
  (append (list (cons (concat "^\\(" (regexp-opt (append bison--declarers bison--directives)) "\\)")
                      '(1 font-lock-keyword-face)))
          c++-font-lock-keywords)
  "Default expressions to highlight in Bison mode.")

;; *************** utilities ***************

(defun just-no-space ()
  "Delete all spaces and tabs around point, leaving no spaces."
  (interactive "*")
  (skip-chars-backward " \t")
  (delete-region (point) (progn (skip-chars-forward " \t") (point)))
  t)

(defun bison--looking-back-has-ws-p ()
  "Return t if there is a whitespace between the beginning of the line and the current (point)."
  (string-match-p "\\s-" (buffer-substring (line-beginning-position) (point))))

(defun bison--looking-back-non-ws-p ()
  "Return t if there are non-whitespace characters between beginning of line and \(point\)."
  (string-match-p "[^ \t]" (buffer-substring (line-beginning-position) (point))))

(defun bison--looking-at-non-ws-p ()
  "Return t if there are non-whitespace characters on the line."
  (looking-at "[^[:cntrl:]]*?[^[:space:][:cntrl:]]"))

(defun line-of-whitespace-p ()
  "Return t if the line consists of nothiing but whitespace, nil otherwise."
  (save-excursion
    (let ((eol (progn (end-of-line) (point))))
      (beginning-of-line)	;; should already be there anyway
      (not (re-search-forward "[^ \t\n]" eol t)))))

(defun bison--electric-p (c) (and c (not bison-electricity-off)))

;; *************** bison-mode ***************

;;;###autoload
(define-derived-mode bison-mode c++-mode "Bison"
  "Major mode for editing bison/yacc files."

  ;; try to set the indentation correctly
  (setq c-basic-offset 4)

  (c-set-offset 'knr-argdecl-intro 0)

  ;; remove auto and hungry anything
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)

  (use-local-map bison-mode-map)

  (define-key bison-mode-map ":" 'bison-electric-:)
  (define-key bison-mode-map "|" 'bison-electric-|)
  (define-key bison-mode-map "{" 'bison-electric-{)
  (define-key bison-mode-map "}" 'bison-electric-})
  (define-key bison-mode-map ";" 'bison-electric-\;)
  (define-key bison-mode-map "%" 'bison-electric-%)
  (define-key bison-mode-map "<" 'bison-electric-lt)
  (define-key bison-mode-map ">" 'bison-electric-gt)

  (define-key bison-mode-map [remap c-indent-command] 'bison-indent-line)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'bison-indent-new-line)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "/*"
	comment-end "*/")
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords nil)
  (set (make-local-variable 'font-lock-defaults) '(bison-font-lock-keywords)))

;;; Section parsers

(defun bison--section ()
  "Return the section that user is currently in."
  (save-excursion
    (save-match-data
      (let ((bound (point))
            (section :init-section))
        ;; To the beginning of a buffer
        (goto-char (point-min))

        ;; Until there is no section delimiters
        (while
            (and (re-search-forward bison--section-open-re bound t)
                 (let ((state (nth (/ (seq-count 'not (cddr (match-data))) 2) bison--sections-dfa)))
                   (if (re-search-forward (nth 1 state) bound t)
                       (setq section (nth 2 state))

                     (setq section (car state))
                     nil))
                 (not (eq section :c-code-section))))

        section))))

;;; Syntax parsers

(defun bison--production-p ()
  "Return t if the \(point\) rests immediately after a production."
  (save-excursion
    (let ((current-point (point)))
      (beginning-of-line)
      (let ((position (re-search-forward
		       bison--production-re current-point t)))
	(and position
	     (not (bison--looking-back-has-ws-p))
	     (= position current-point))))))

(defun bison--find-production-opener ()
  "Return and goto the point of the nearest production opener above \(point\)."
  (re-search-backward bison--production-re nil t))

(defun bison--find-point (re)
  (save-excursion
    (when (re-search-forward re nil t)
	  (beginning-of-line)
	  (point))))

(defun bison--find-next-production ()
  "Return the position of the beginning of the next production, or nil if there isnt one."
  (bison--find-point bison--production-re))

(defun bison--find-grammar-end ()
  "Return the position of the end of the grammar rules (assuming we are within the grammar rules section), or nil if there isnt one."
  (bison--find-point bison--rules-section-delimeter))

(defun bison--find-grammar-begin ()
  "Return the position of the beginning of the grammar rules (assuming we are within the grammar rules section), or nil if there isnt one."
  (save-excursion
    (if (re-search-backward bison--rules-section-delimeter nil t)
	(point))))

(defun bison--within-started-production-p ()
  "Used by bison-electric-* functions to determine actions.

Return t if within a production, nil if not.
A point is within a production if there is some non whitespace text before
either the beginnings of another production or the end of the grammar rules."
  (save-excursion
    (let ((bound (cond ((bison--find-next-production))
		       ((bison--find-grammar-end))
		       (t nil))))
      (if bound
	  (let ((sval (re-search-forward
		       (concat "\\(\\s \\|" ;; whitespace or
					    ;; comments
			       (regexp-quote comment-start)
			       "\\(.\\|\n\\)*" ;; comment body
			       (regexp-quote comment-end)
			       "\\)+")	;; end or
		       bound t)))
	    (if sval
		(not (= sval bound))
	      nil))
	nil))))

(defun bison--within-some-sexp-p (starter ender)
  "Return t if the \(point\) is within the sexp marked by the re's STARTER and ENDER."
  (save-excursion
    (let ((current-point (point)))
      (if (re-search-backward starter nil t) ;; find nearest starter
	  ;; look for ender, if found, then not within sexp
	  (progn
	    (goto-char (match-end 0))
	    (not (re-search-forward ender current-point t)))))))

(defun bison--within-c-comment-p ()
  "Return t if the point is within a c comment delimited by \"/*\" \"*/\"."
  (bison--within-some-sexp-p (regexp-quote comment-start)
			     (regexp-quote comment-end)))


(defun bison--within-string-p (&optional point)
  "Start from the beginning of the buffer and toggle state as un-escaped \"'s are found."
  (let ((point (or point (point)))
	(in-p nil))
    (save-excursion
      (goto-char (point-min))

      (while (re-search-forward "[^\\]\"" point t)
	(setq in-p (not in-p)))

      in-p)))

;;; bison--within-braced-c-expression-p
;;; new and improved, no more recursion, does not break when literal strings
;;; contain un-matched braces
(defun bison--within-braced-c-expression-p (section)
  "return t if the point is within an sexp delimited by braces \({,}\)"
  (save-excursion
    (bison--within-braced-c-expression-p-h section (point))))

(defun bison--within-braced-c-expression-p-h (section low-pt)
  "save excursion is done higher up, so i dont concern myself here."
  (cond ((eq section :init-section) nil)
	((eq section :c-decls-section)
	 (let ((opener (save-excursion (search-backward "%{"))))
	   (bison--within-braced-c-expression-p-h-h opener low-pt)))
	((eq section :bison-decls-section)
	 (let ((opener (save-excursion
			 (or (search-backward "%}" nil t)
			     (point-min)))))
	   (bison--within-braced-c-expression-p-h-h opener low-pt)))
	((eq section :rules-section)
	 (let ((opener (save-excursion (bison--find-production-opener))))
	   (if opener
	       (bison--within-braced-c-expression-p-h-h opener low-pt)
	     nil)))
	((eq section :c-code-section)
	 t)))

(defun bison--within-braced-c-expression-p-h-h (high-pt low-pt)
  "
Notes:
HIGH-PT goes toward (point-min), LOW-PT goes toward (point-max)
save excursion is done higher up, so i dont concern myself here.
"
  (let ((pt (point)))
    (let ((success nil) (count 1) (done nil))
      ;; loop until open brace found, that is not in comment or string literal
      (while (and (not done)
		  (re-search-backward "[^%]{" high-pt t count)) ;find nearest
							        ;starter
	(goto-char (match-end 0))
	(if (or (bison--within-c-comment-p)
		(bison--within-string-p))

	    (setq count (+ count 1))
	  (progn
	    (setq success t)
	    (setq done t))))

      (if success
	  (let ((end-pt
		 (condition-case nil
		     (progn
                       (backward-char)
                       (forward-sexp)
                       (point))
		   (error nil))))
	    (if end-pt
		(if (> end-pt low-pt)
		    t			; then in braced-c-exp
		  nil)
	      t))			; if no sexp close brace, then w/in
	nil))))


(defun bison--bison-decl-opener-p (bol eol)
  "return t if the current line is a bison declaration starter \(i.e. has a %type, %token, %right, ...\)"
  (save-excursion
    (goto-char bol)
    (re-search-forward
     (concat "^" (regexp-opt (copy-sequence bison--declarers))) eol t)))

(defun bison--production-opener-p (bol eol)
  "return t if the current line is a line that introduces a new production"
  (save-excursion
    (goto-char bol)
    (re-search-forward bison--production-re eol t)))

(defun bison--find-bison-semicolon ()
  "return the position of next semicolon not within braces, nil otherwise"
  (save-excursion
    (if (search-forward ";" nil t)
	(if (not (bison--within-braced-c-expression-p (bison--section)))
	    (point)
	  (bison--find-bison-semicolon))
      nil)))

(defun bison--within-production-body-p (section)
  "Return t if the \(point\) is within the body of a production.
This procedure will fail if it is in a production header."
  (save-excursion
    (and (eq section :rules-section)
         (re-search-backward bison--production-re nil t)
         t)))

(defun bison--production-alternative-p (bol eol section)
  "Return t if the current line contains a \"|\" used to designate a rule alternative."
  (save-excursion
    (goto-char bol)
    (and (re-search-forward "|\\|\\(?:^\\s-*;\\s-*$\\)" eol t)
         (not (bison--within-braced-c-expression-p section)))))

;; *************** indent functions ***************

(defun bison--handle-indent-c-sexp (section indent-column bol)
  (let* ((o-brace (re-search-backward "[^%]{" bol t)))
    (if o-brace
	(if (save-excursion
	      (goto-char o-brace)
	      (bison--within-braced-c-expression-p section))
	    (c-indent-line)
	  (if (= (current-indentation) o-brace)	;; if o-brace is first char
	      (if (not (= o-brace indent-column)) ;; but not in right spot
		  (progn
		    (back-to-indentation)
		    (just-no-space)
		    (indent-to-column indent-column))
		;; else all is good
		)
	    ;; else, non-ws before o-brace, leave it alone
	    ))
      (c-indent-line))))

(defun bison-indent-new-line (&optional c-sexp)
  "Indent a fresh line of bison code.
Assumes indenting a new line, i.e. at column 0.
"
  (interactive)

  (let* ((section (bison--section))
	 (c-sexp (or c-sexp (bison--within-braced-c-expression-p section))))
    (cond
     (c-sexp
      (cond
       ((eq section :rules-section)
	(c-indent-line
	 (save-excursion
	   (forward-line -1)
	   (let ((bol (save-excursion (beginning-of-line) (point)))
		 (eol (save-excursion (end-of-line) (point))))
	     (if (bison--production-opener-p bol eol)
		 (list
		  (cons 'defun-block-intro
			(progn
			  (re-search-forward bison--production-re) ; SIGERR
			  (- (re-search-forward "[^ \t]") ; SIGERR
			     1))))
	       nil)))))
      (t (c-indent-line))))
     ((eq section :init-section)
      (c-indent-line))
     ((eq section :bison-decls-section)
      (indent-to-column bison-decl-token-column))
     ((eq section :rules-section)
      (indent-to-column
       (save-excursion
	 (let* ((bound (or (save-excursion (bison--find-production-opener))
			   (bison--find-grammar-begin)))
		(prev-semi (search-backward ";" bound t))
		)
	   (if prev-semi
	       (if (bison--within-braced-c-expression-p section) ; CRACK
		   bison-rule-enumeration-column
		 0)
	     (if (save-excursion (bison--find-production-opener))
		 bison-rule-enumeration-column
	       0))))))
     ((eq section :c-code-section)) ;;leave-alone
     )))

(defun bison-indent-line ()
  "Indent a line of bison code."
  (interactive)

  (let* ((pos (- (point-max) (point)))
	 (reset-pt (function (lambda ()
			       (if (> (- (point-max) pos) (point))
				   (goto-char (- (point-max) pos))))))
	 (bol (save-excursion (beginning-of-line) (point)))
	 (eol (save-excursion (end-of-line) (point))))

    (let* ((section (bison--section))
	   (c-sexp (bison--within-braced-c-expression-p section))
	   (ws-line (line-of-whitespace-p)))
      (cond
       ;; if you are a line of whitespace, let indent-new-line take care of it
       (ws-line
	(bison-indent-new-line c-sexp))

       ((eq section :c-decls-section)
	(if c-sexp
	    (bison--handle-indent-c-sexp section 0 bol)
	  (if (not (= (current-indentation) 0))
	      (progn
		(back-to-indentation)
		(just-no-space)
		(funcall reset-pt)))))

       ((or (eq section :bison-decls-section)
            (eq section :init-section))
	(let ((opener (bison--bison-decl-opener-p bol eol)))
	  (cond
	   (opener
	    (goto-char opener)
	    (skip-chars-forward " \t" eol)
	    (if (looking-at "{")
		(save-excursion
		  (if (bison--looking-at-non-ws-p)
		      (progn
			(forward-char 1)
			(just-no-space)
			(newline)
			(bison-indent-new-line t))))
	      (let ((complete-type t))
		(if (looking-at "<")
		    (progn
		      (setq complete-type nil)
		      (if (not (= (current-column) bison-decl-type-column))
			  (progn
			    (just-no-space)
			    (indent-to-column bison-decl-type-column))
			(and (re-search-forward
			      (concat "<" bison--word-constituent-re "+>")
			      eol t)
			     (setq complete-type t)))))
		(and complete-type
		     (skip-chars-forward " \t" eol)
		     (looking-at
		      (concat "\\(" bison--word-constituent-re "\\|'\\)"))
		     (if (not (= (current-column) bison-decl-token-column))
			 (progn
			   (just-no-space)
			   (indent-to-column bison-decl-token-column))))))
	    (funcall reset-pt))
	   (c-sexp
	    (bison--handle-indent-c-sexp section 0 bol))
	   (t
	    (back-to-indentation)
	    ;; only tab in names, leave comments alone
	    (cond (;; put word-constiuents in bison-decl-token-column
		   (looking-at bison--word-constituent-re)
		   (if (not (= (current-column) bison-decl-token-column))
		       (progn
			 (just-no-space)
			 (indent-to-column bison-decl-token-column))))
		  ;; put/keep close-brace in the 0 column
		  ((looking-at "}")
		   (if (not (= (current-column) 0))
		       (just-no-space)))
		  ;; leave comments alone
		  ((looking-at (regexp-quote comment-start)) nil)
		  ;; else do nothing
		  )
	    (funcall reset-pt)))))
       ((eq section :rules-section)
	(cond

	 ((bison--production-opener-p bol eol)
	  (beginning-of-line)
	  (re-search-forward bison--production-re);; SIGERR
	  (when (looking-at bison--action-re)
            (if (> (current-column) bison-rule-enumeration-column)
                (progn
                  (just-no-space)
                  (newline)
                  (indent-to-column bison-rule-enumeration-column))
              (save-excursion
                ;; Here the match data from looking at bison--action-re
                (goto-char (nth 2 (match-data)))
                (when (> (current-column) bison-rule-enumeration-column)
                  (just-no-space))
                (indent-to-column bison-rule-enumeration-column))))
	  (funcall reset-pt))

	 ((bison--production-alternative-p bol eol section)
	  (back-to-indentation);; should put point on "|"
	  (if (not (= (current-column) bison-rule-separator-column))
	      (progn
		(just-no-space)
		(indent-to-column bison-rule-separator-column)))
	  (forward-char 1)
	  (if (bison--looking-at-non-ws-p)
	      (save-excursion
		(re-search-forward bison--word-constituent-re);; SIGERR
		(let ((col (current-column)))
		  (cond ((> col (+ 1 bison-rule-enumeration-column))
			 (forward-char -1)
			 (just-no-space)
			 (indent-to-column bison-rule-enumeration-column))
			((< col (+ 1 bison-rule-enumeration-column))
			 (forward-char -1)
			 (indent-to-column
			  bison-rule-enumeration-column))))))
	  (funcall reset-pt))
	 (c-sexp
	  (bison--handle-indent-c-sexp
	   section bison-rule-enumeration-column bol)
	  (funcall reset-pt))
	 ((bison--within-production-body-p section)
	  (back-to-indentation)
	  (if (not (= (current-column) bison-rule-enumeration-column))
	      (progn
		(just-no-space)
		(indent-to-column
		 bison-rule-enumeration-column)))
	  (funcall reset-pt))
	 (t
	  (let ((cur-ind (current-indentation)))
	    (if (eq (save-excursion (search-backward "}" bol t))
		    cur-ind)
		(if (not (= cur-ind bison-rule-enumeration-column))
		    (progn
		      (back-to-indentation)
		      (just-no-space)
		      (indent-to-column bison-rule-enumeration-column)
		      (funcall reset-pt)))
	      ;; else leave alone
	      )))))
       ((eq section :c-code-section)
	(c-indent-line))
       ))))

;; *************** electric-functions ***************

(defun bison-electric-: (arg)
  "Insert a colon character.
If the colon <:> delineates a production,
then insert a semicolon on the next line in the BISON-RULE-SEPARATOR-COLUMN,
    put the cursor in the BISON-RULE-ENUMERATION-COLUMN for the beginning
    of the rule
else just run self-insert-command
A colon delineates a production by the fact that it is immediately preceded by
a word(alphanumerics or '_''s), and there is no previous white space.
"
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))
  (when (bison--electric-p bison-electric-:)
      (if (and (eq :rules-section (bison--section))
	       (bison--production-p)
	       (not (bison--within-started-production-p)))
	  (progn
	    (save-excursion		; put in a closing semicolon
	      (newline)
	      (indent-to-column bison-rule-separator-column)
	      (insert ";"))
	    (save-excursion		; remove opening whitespace
	      (if (re-search-backward
		   "\\s "
		   (save-excursion (beginning-of-line) (point))
		   t)
		  (just-no-space)))
	    (if (not (< (current-column) bison-rule-enumeration-column))
		(newline))
	    (indent-to-column bison-rule-enumeration-column)))))

(defun bison-electric-| (arg)
  "Insert a \"|\" character.
If the pipe <|> is used as a rule separator within a production,
then move it into BISON-RULE-SEPARATOR-COLUMN
    indent to BISON-RULE-ENUMERATION-COLUMN on the same line
else just run self-insert-command
"
  (interactive "P")

  (if (and (bison--electric-p bison-electric-|)
	   (eq :rules-section (bison--section))
	   (line-of-whitespace-p))
      (progn
	(beginning-of-line)
	(just-no-space)
	(indent-to-column bison-rule-separator-column)
	(self-insert-command (prefix-numeric-value arg))
	(indent-to-column bison-rule-enumeration-column))

    (self-insert-command (prefix-numeric-value arg))))

(defun bison-electric-{ (arg)
  "Insert and opening brace \"{\".
When the brace opens a C action definition for production rules,
if there is only whitespace before \(point\), then put open-brace in
bison-rule-enumeration-column."
  (interactive "P")

  (when (bison--electric-p bison-electric-{)
      (let ((section (bison--section)))
	(cond ((and (eq section :rules-section)
		    (not (bison--within-braced-c-expression-p section))
		    (not (bison--looking-back-non-ws-p)))
	       (if (not (= (current-column) bison-rule-enumeration-column))
		   (progn
		     (just-no-space)
		     (indent-to-column bison-rule-enumeration-column))))
	      ((and (eq section :bison-decls-section)
		    (not (bison--within-braced-c-expression-p section))
		    (not (bison--looking-back-non-ws-p)))
	       (if (not (= (current-column) 0))
		   (progn
		     (just-no-space)
		     (indent-to-column 0)))))))

  (self-insert-command (prefix-numeric-value arg)))


(defun bison-electric-} (arg)
  "If the close-brace \"}\" is used as the c-declarations section closer
in \"%}\", then make sure the \"%}\" indents to the beginning of the line"
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))

  (when (bison--electric-p bison-electric-})
      (cond ((search-backward "%}" (- (point) 2) t)
	     (if (eq (bison--section) :c-decls-section)
		 (progn
		   (just-no-space)
		   (forward-char 2))	; for "%}"
	       (forward-char 1)))
	    )))

(defun bison-electric-\; (arg)
  "if the semicolon is used to end a production, then place it in
bison-rule-separator-column

a semicolon is deemed to be used for ending a production if it is not found
within braces

this is just self-insert-command as i have yet to write the actual
bison-electric-\; function yet
"
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg)))

(defun bison-electric-% (arg)
  "If the percent is a declarer in the bison declaration's section,
then put it in the 0 column."
  (interactive "P")

  (when (bison--electric-p bison-electric-%)
      (let ((section (bison--section)))
	(if (and (eq section :bison-decls-section)
		 (not (bison--within-braced-c-expression-p section))
		 (not (bison--looking-back-non-ws-p))
		 (not (= (current-column) 0)))
	    (just-no-space))))

  (self-insert-command (prefix-numeric-value arg)))

(defun bison-electric-lt (arg)
  "If the less-than is a type declarer opener for tokens in the bison
declaration section, then put it in the bison-decl-type-column column."
  (interactive "P")

  (when (bison--electric-p bison-electric-lt)
      (if (and (eq (bison--section) :bison-decls-section)
	       (bison--bison-decl-opener-p
		(save-excursion (beginning-of-line) (point))
		(point)))
	  (progn
	    (just-no-space)
	    (indent-to-column bison-decl-type-column))))

  (self-insert-command (prefix-numeric-value arg)))

(defun bison-electric-gt (arg)
  "If the greater-than is a type declarer closer for tokens in the bison
declaration section, then indent to bison-decl-token-column."
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))

  (when (bison--electric-p bison-electric-gt)
      (let ((current-pt (point))
	    (bol (save-excursion (beginning-of-line) (point))))
	(if (and (eq (bison--section) :bison-decls-section)
		 (bison--bison-decl-opener-p bol (point)))
	    (if (search-backward "<" bol t)
		(if (re-search-forward
		     (concat "<" bison--word-constituent-re "+>")
		     current-pt t)
		    (if (not (bison--looking-at-non-ws-p))
			(progn
			  (just-no-space)
			  (indent-to-column bison-decl-token-column)))))))))

;;;###autoload
(define-derived-mode jison-mode bison-mode
  "Major mode for editing jison files.")

(provide 'bison-mode)
(provide 'jison-mode)
;;; bison-mode.el ends here
