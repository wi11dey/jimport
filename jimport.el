;;; jimport.el --- Java imports that follow you around -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: java import

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'syntax)

(defun jimport--match-string-no-properties (num)
  (when (match-beginning num)
    (buffer-substring-no-properties (match-beginning num)
				    (match-end       num))))

(defvar jimport--import-regexp
  (rx word-start
      (or "package"
	  (group-n 1 "import"))
      (1+ space)
      (group-n 2
	(optional
	 "static"
	 (1+ space))
	(0+ (1+ word) ?.)
	(group-n 3
	  (1+ word)))
      (0+ space)
      ?\;))

(defconst jimport--ignore
  (eval-when-compile
    (let ((set (make-hash-table :test #'equal)))
      (puthash "Appendable" t set)
      (puthash "ArithmeticException" t set)
      (puthash "ArrayIndexOutOfBoundsException" t set)
      (puthash "ArrayStoreException" t set)
      (puthash "AutoCloseable" t set)
      (puthash "Boolean" t set)
      (puthash "Byte" t set)
      (puthash "CharSequence" t set)
      (puthash "Character" t set)
      (puthash "Class" t set)
      (puthash "ClassCastException" t set)
      (puthash "ClassLoader" t set)
      (puthash "ClassNotFoundException" t set)
      (puthash "ClassValue" t set)
      (puthash "CloneNotSupportedException" t set)
      (puthash "Cloneable" t set)
      (puthash "Comparable" t set)
      (puthash "Compiler" t set)
      (puthash "Deprecated" t set)
      (puthash "Double" t set)
      (puthash "Enum" t set)
      (puthash "EnumConstantSupportedException" t set)
      (puthash "Exception" t set)
      (puthash "Float" t set)
      (puthash "FunctionalInterface" t set)
      (puthash "IllegalAccessException" t set)
      (puthash "IllegalCallerException" t set)
      (puthash "IllegalMonitorStateException" t set)
      (puthash "InheritableThreadLocal" t set)
      (puthash "Integer" t set)
      (puthash "Iterable" t set)
      (puthash "Long" t set)
      (puthash "Math" t set)
      (puthash "Module" t set)
      (puthash "ModuleLayer" t set)
      (puthash "Number" t set)
      (puthash "Object" t set)
      (puthash "Override" t set)
      (puthash "Package" t set)
      (puthash "Process" t set)
      (puthash "ProcessBuilder" t set)
      (puthash "ProcessHandle" t set)
      (puthash "Readable" t set)
      (puthash "Runnable" t set)
      (puthash "Runtime" t set)
      (puthash "RuntimePermission" t set)
      (puthash "SafeVarargs" t set)
      (puthash "SecurityManager" t set)
      (puthash "Short" t set)
      (puthash "StackTraceElement" t set)
      (puthash "StackWalker" t set)
      (puthash "StrictMath" t set)
      (puthash "String" t set)
      (puthash "StringBuffer" t set)
      (puthash "StringBuilder" t set)
      (puthash "System" t set)
      (puthash "System" t set)
      (puthash "Thread" t set)
      (puthash "ThreadGroup" t set)
      (puthash "ThreadLocal" t set)
      (puthash "Throwable" t set)
      (puthash "Void" t set)
      set))
  "Imports that should not be automatically added, such as those from `java.lang.*'.")

(defun jimport--imports ()
  (let ((imports (copy-hash-table (make-hash-table :test #'equal))))
    (save-excursion
      (without-restriction
	(goto-char (point-min))
	(save-match-data
	  (while (re-search-forward jimport--import-regexp nil t)
	    (puthash (and (match-beginning 1)
			  (jimport--match-string-no-properties 3))
		     (jimport--match-string-no-properties 2)
		     imports)))))
    imports))

(defun jimport--yank-handler (string-and-buffer)
  (let ((start (point))
	end)
    (insert (car string-and-buffer))
    (setq end (point))
    (unless (or (not (derived-mode-p 'java-mode))
		(eq (cdr string-and-buffer) (current-buffer))
		(minibufferp))
      (let (;; Current file:
	    (our-imports (jimport--imports))
	    ;; File being yanked from:
	    (their-imports (with-current-buffer (cdr string-and-buffer)
			     (jimport--imports)))
	    (case-fold-search nil)
	    (imported (make-hash-table :test #'equal)))
	(save-excursion
	  (save-match-data
	    (goto-char start)
	    (while (re-search-forward (rx-let ((identifier (any (?A . ?Z)
								(?a . ?z)
								(?0 . ?9)
								?$
								?_)))
					(rx (intersection identifier (not (any (?0 . ?9))))
					    (0+ identifier)))
				      end
				      t)
	      (save-excursion
		(goto-char (match-beginning 0))
		(unless (or (eq (preceding-char) ?.) ; Qualified name.
			    (nth 8 (syntax-ppss)))
		  (let* ((symbol (jimport--match-string-no-properties 0))
			 (our-import   (gethash symbol our-imports))
			 (their-import (gethash symbol their-imports))
			 (import (cond
				  ((not their-import)
				   ;; Assume that they were implicitly referencing a symbol from their own package, so if we are in a different package we need to convert their implicit import to an explicit import.
				   (unless (gethash symbol jimport--ignore) ; Ignore if needed, e.g. if the import is from `java.lang.*'.
				     (let ((our-package   (gethash nil our-imports))
					   (their-package (gethash nil their-imports)))
				       (when (and their-package ; If they have no package declaration, don't do anything.
						  (not (equal our-package their-package)) ; We already implicitly import the symbol if we're in the same package.
						  (looking-at-p (rx upper lower)) ; Don't convert implicit imports to explicit imports when they don't start with an uppercase letter followed by a lowercase letter, since they are probably methods, constants, or variables instead. By Java convention, only classes are in CamelCaps.
						  )
					 (concat their-package "." symbol)))))
				  ;; They have the import.
				  ((equal our-import their-import)
				   ;; Already imported, do nothing.
				   nil)
				  ;; We either don't have the import, or it's different than theirs.
				  (t
				   ;; In both cases, yank their import as well.
				   their-import))))
		    (when import
		      (puthash import t imported))))))
	    (when (> (hash-table-count imported) 0)
	      (without-restriction
		(let ((imported-start (point))
		      imported-end)
		  (if (re-search-backward jimport--import-regexp nil :noerror)
		      (progn
			(goto-char (match-end 0))
			(unless (match-beginning 1)
			  (newline)))
		    (forward-comment (buffer-size))
		    (open-line 2))
		  (maphash (lambda (import _value)
			     (newline)
			     (insert "import " import ";"))
			   imported)
		  (setq imported-end (point)
			yank-undo-function (lambda (start end)
					     (delete-region start end)
					     (delete-region imported-start imported-end))))))))))))

;; TODO: don't add yank-handler on template args or comments
(defun jimport--filter-buffer-substring (substring)
  (if (derived-mode-p 'java-mode)
      (propertize substring 'yank-handler (list #'jimport--yank-handler
						(cons substring (current-buffer))))
    substring))

(define-minor-mode jimport-mode
  "Minor mode which yanks necessary imports as well when you yank code between `java-mode' buffers."
  :global t
  :lighter " import"
  :group 'c
  ;;;; Teardown
  (remove-function filter-buffer-substring-function #'jimport--filter-buffer-substring)
  ;;;; Construction
  (when jimport-mode
    (add-function :filter-return filter-buffer-substring-function #'jimport--filter-buffer-substring)))



(defun jimport--get-current-header ()
  (save-excursion
    (without-restriction
      (goto-char (point-min))
      (forward-comment (buffer-size))
      (save-match-data
	(when (and (looking-at jimport--import-regexp)
		   (not (match-beginning 1)) ; Package declaration.
		   )
	  (goto-char (match-end 0))))
      (forward-comment (buffer-size))
      (let ((header (buffer-substring (point-min) (point))))
	(unless (string-blank-p header)
	  header)))))

(defcustom jimport-root-packages
  '("com"
    "org"
    "net"
    "edu"
    "gov"
    "mil")
  ""
  :type '(repeat string))

(defun jimport-guess-header ()
  "Try to intelligently guess the header of the current file using the following strategy:

1) First, look at other Java files in the current directory to deduce the header.
2) If that fails, look through the path components of the current file to find common root packages (given by `jimport-root-packages', which see).
3) Return nil."
  (let ((current-file-name (file-name-nondirectory (buffer-file-name))))
    (with-temp-buffer
      (java-mode)
      (named-let check-other-files ((files (directory-files default-directory
							    nil
							    (rx ?. "java" string-end))))
	(if (consp files)
	    (or (unless (equal (car files) current-file-name)
		  (ignore-errors
		    (insert-file-contents (car files) :visit nil nil :replace)
		    (jimport--get-current-header)))
		(check-other-files (cdr files)))
	  (named-let descend ((components (file-name-split current-file-name)))
	    (cond
	     ((not components)
	      nil)
	     ((member (car components)
		      )
	      (concat "package "
		      (string-join components ".")
		      ";\n\n"))
	     (t
	      (descend (cdr components))))))))))

(define-minor-mode jimport--auto-package-mode
  "Minor mode to intelligently add package declarations and headers to the newly created `java-mode' files based on other files in the same directory."
  :lighter nil
  (when (and jimport--auto-package-mode
	     (derived-mode-p 'java-mode)
	     (not (file-exists-p (buffer-file-name)))
	     (not (jimport--get-current-header)))
    (save-excursion
      (without-restriction
	(goto-char (point-min))
	(let ((header (jimport-guess-header)))
	  (when header
	    (skip-chars-forward " \t\n")
	    (delete-region (point-min) (point))
	    (insert header)))))))

(define-global-minor-mode jimport-auto-package-mode jimport--auto-package-mode jimport--auto-package-mode
  :predicate '(java-mode))

(provide 'jimport)

;;; jimport.el ends here
