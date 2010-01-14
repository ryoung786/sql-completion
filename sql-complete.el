;;; sql-complete.el --- provide completion for tables and columns

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Ryan Young <ryoung786@gmail.com>
;; Version: 0.0.2
;; Keywords: comm languages processes

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Trying to provide a framework for completion that will eventually
;; make it into sql.el.

;; It takes a while to build the completions up.  The idea is to send
;; the select macro to the interpreter, then parse the output after
;; it writes to the buffer.  However, I've noticed that I have to
;; wait for the output to be completely written to the buffer before
;; I can move on to parse, otherwise we won't get the full list of
;; values.  With that in mind, adjust the wait times to suit your
;; needs, based on how large your db schema is.


 

;;; Code:

(require 'sql)

(defcustom sql-db2-table2column-macro
  "select '(\"' || c.TABNAME || '\" \"' || c.COLNAME || '\")' from syscat.tables t, syscat.columns c where t.TABNAME = c.TABNAME and t.TYPE='T' and t.OWNERTYPE='U' order by c.TABNAME"
  "SQL Statement to determine all tables and columns."
  :group 'SQL
  :type 'string)

(defcustom sql-db2-schema2table-macro
  "select '(\"' || tabschema || '\" \"' || tabname || '\")' from syscat.tables order by tabschema"
  "SQL Statement to determine all tables and columns."
  :group 'SQL
  :type 'string)

(defcustom sql-db2-schema2procedure-macro
  "select '(\"' || PROCSCHEMA || '\" \"' || PROCNAME || '\")' from SYSCAT.PROCEDURES where definer not in ('SYSIBM')"
  "SQL statement to determine the names of all the valid stored procedures"
  :group 'SQL
  :type 'string)
;; backends

(defun generate-alist (statement timeout)
  "Return table and columns from the Data Dictionary using SQL.
STATEMENT must be a SQL statement that returns the data dictionary
one column per line.  Each line must look like this:

\(\"table-name\" \"column-name\")

Any lines not looking like this will be skipped to allow for column
headers and other fancy markup.

This currently depends very much on a good `comint-prompt-regexp'.

TIMEOUT is the approx time it will take to dump the results of
STATEMENT into the buffer [in seconds]"
  (when (null sql-buffer)
    (error "No SQLi buffer available"))
  (save-excursion
    (set-buffer sql-buffer)
    (let (result end)
      (insert "ls")
      (comint-send-input)
      (insert statement)
      (comint-send-input)
      (end-of-buffer)
      ; we have to wait 20 secs b/c we don't
      ; know when the comint buffer is done printing the results
      (sit-for timeout)
      (comint-previous-prompt 1)
      (while (= 0 (forward-line 1))
	(when (looking-at "^(.*)")
	  (let* ((entry-a (car (read-from-string (match-string 0))))
		 (table (chomp (car entry-a)))
		 (column (chomp (cadr entry-a)))
		 (entry (list table column))
		 (item (cdr (assoc table result))))
	    (if item
		(nconc item (list column))
	      (setq result (append (list entry) result))))))
      result)))

;; framework

(defvar sql-table2col-alist nil
  "The data dictionary to use for completion.
Each element of the list has the form
\(TABLE COLUMN1 COLUMN2 ...)")

(defvar sql-schema2table-alist nil
  "The data dictionary to use for completion.
Each element of the list has the form
\(TABLE COLUMN1 COLUMN2 ...)")

(defun sql-build-completions ()
  "Build the sql completion a-lists"
  (interactive)
  (setq sql-schema2table-alist
	(generate-alist sql-db2-schema2table-macro 10))
  (message "SQL schema -> table completion done initializing")
  (setq sql-table2col-alist
	(generate-alist sql-db2-table2column-macro 50))
  (message "SQL table -> column completion done initializing"))

(defun sql-column-completions ()
  (let* ((word-array (split-string (get-input) "[\\. \n]"))
	 (prefix (upcase (car (last word-array))))
	 (table_name (upcase (chomp (get-2nd-to-last word-array))))
	 (completions (cdr (assoc table_name sql-table2col-alist))))
    (if (not completions)
	; now we need to look through the row to find all instances of the alias
	; pick out all the previous words (after the .)
	; and see which one is in the table_names, then use that one for the key
	; to the table->col alist
	(let* ((alias table_name)
	       (table_names (mapcar 'car sql-table2col-alist))
	       (line-words (split-string (get-input) "[\\. \n]" t))
	       (found-table nil))
	  (catch 'break
	    (while (cadr line-words)
	      (when (and (string= (upcase (cadr line-words)) alias)
			 (member (upcase (car line-words)) table_names))
		(setq found-table (upcase (car line-words)))
		(throw 'break nil))
	      (setq line-words (cdr line-words))
	      (message "%S" line-words)))
	  (setq completions (cdr (assoc found-table sql-table2col-alist)))))
    completions))
;    (comint-dynamic-simple-complete prefix completions)))

(defun get-2nd-to-last (somelist)
  (while (> (list-length somelist) 2) (setq somelist (cdr somelist)))
  (car somelist))

(defun sql-table-completions ()
  (let* ((word-array (split-string (pick-out-word-before-period) "\\."))
	 (prefix (upcase (car (last word-array))))
	 (schema_name (upcase (chomp (get-2nd-to-last word-array)))))
    (cdr (assoc schema_name sql-schema2table-alist))))

(defun sql-schema-completions ()
  (mapcar 'car sql-schema2table-alist))

(defun sql-complete ()
  "Attempts to complete the word from known
schema, table, and column names.  Must call sql-build-completions once before this will be useful"
  (interactive)
  (let* ((word-array (split-string (pick-out-word-before-period) "\\."))
	 (prefix (upcase (chomp (car (last word-array)))))
	 (foo (list (sql-column-completions) (sql-table-completions) (sql-schema-completions))))
    (while (and foo
		(not (car foo)))
      (setq foo (cdr foo)))
    (comint-dynamic-simple-complete prefix (car foo))))

(defun get-input()
  "Gets the string that, if the user were to hit enter,
   it would be sent to the process"
  (save-excursion
    (comint-goto-process-mark)
    (message (buffer-substring (point) (point-max)))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR.

Why doesn't Emacs have this built in? --Ripped from dhax (of ezbl fame)"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (save-excursion
      ;; Make the [:space:] class match newline.
      (with-syntax-table (copy-syntax-table)
        (modify-syntax-entry ?\n " ")
        (string-match "^[[:space:]]*\\(.*?\\)[[:space:]]*$" s)
        (match-string 1 s)))))

(defun pick-out-word-before-period ()
  (interactive)
  ; grab the text 
  (let* ((line (thing-at-point 'line))
	 (p1 nil)      ; p1 is the left-most point
	 (p2 (point))) ; p2 is the right-most point
    (re-search-backward " ")
    (setq p1 (point))    
    (message (buffer-substring p1 p2))
    (goto-char p2)
    (buffer-substring p1 p2)))

;;; sql-complete.el ends here