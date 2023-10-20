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

(defun jimport--match-string-no-properties (num)
  (when (match-beginning num)
    (buffer-substring-no-properties (match-beginning num)
				    (match-end       num))))

(defvar jimport--header-regexp
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
      set)))

(defun jimport--imports ()
  (let ((imports (copy-hash-table (make-hash-table :test #'equal))))
    (save-excursion
      (without-restriction
	(goto-char (point-min))
	(save-match-data
	  (while (re-search-forward jimport--header-regexp nil t)
	    (puthash (and (match-beginning 1)
			  (jimport--match-string-no-properties 3))
		     (jimport--match-string-no-properties 2)
		     imports)))))
    imports))

(defun jimport--yank-handler (string-and-buffer)
  (when (derived-mode-p 'java-mode)
    (let ((start (point))
	  end)
      (insert (car string-and-buffer))
      (setq end (point))
      (unless (eq (cdr string-and-buffer) (current-buffer))
	(let ((our-imports (jimport--imports))
	      (their-imports (with-current-buffer (cdr string-and-buffer)
			       (jimport--imports)))
	      (case-fold-search nil)
	      imported)
	  (save-excursion
	    (save-match-data
	      (goto-char start)
	      (while (re-search-forward (rx-let ((identifier (any (?A . ?Z)
								  (?a . ?z)
								  (?0 . ?9)
								  ?$
								  ?_)))
					  (rx (intersection identifier (not (?0 . ?9)))
					      (0+ identifier)))
					end
					t)
		(let* ((symbol (jimport--match-string-no-properties 0))
		       (import (or (gethash symbol their-imports)
				   (unless (or (gethash symbol jimport--ignore)
					       (gethash symbol our-imports))
				     (save-excursion
				       (goto-char (match-beginning 0))
				       (let ((our-package   (gethash nil our-imports))
					     (their-package (gethash nil their-imports)))
					 (when (and their-package
						    (not (equal our-package their-package))
						    (looking-at (rx (any (?A . ?Z)))))
					   (concat their-package "." symbol))))))))
		  (when import
		    (push import imported))))
	      (when imported
		(without-restriction
		  (if (re-search-backward jimport--header-regexp nil :noerror)
		      (progn
			(goto-char (match-end 0))
			(unless (match-beginning 1)
			  (newline))
			(newline))
		    (forward-comment (buffer-size))
		    (open-line 2))
		  (let ((imported-start (point))
			imported-end)
		    (insert (mapconcat (lambda (import)
					 (concat "import " import ";"))
				       imported
				       "\n"))
		    (setq imported-end (point)
			  yank-undo-function (lambda (start end)
					       (delete-region start end)
					       (delete-region imported-start imported-end)))))))))))))

;; TODO: don't add yank-handler on template args or comments
(defun jimport--filter-buffer-substring (substring)
  (if (derived-mode-p 'java-mode)
      (propertize substring 'yank-handler (list #'jimport--yank-handler
						(cons substring (current-buffer))))
    substring))

(define-minor-mode jimport-mode
  ""
  :global t
  :lighter " import"
  :group 'c
  ;;;; Teardown
  (remove-function filter-buffer-substring-function #'jimport--filter-buffer-substring)
  ;;;; Construction
  (when jimport-mode
    (add-function :filter-return filter-buffer-substring-function #'jimport--filter-buffer-substring)))

(provide 'jimport)

;;; jimport.el ends here
