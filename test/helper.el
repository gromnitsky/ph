;; -*- lexical-binding: t -*-

(require 'cl-lib)

(setq argv (cdr argv))					; remove '--' from CL arguments

(setq ph-verbose 0)

(setq tdd-start-dir default-directory)
(setq tdd-work-dir (concat tdd-start-dir "tmp" ))
(message (format "wd == %s" tdd-work-dir))

(setq tdd-y-or-n t)

(defun tdd-setup-global ()
  (cd tdd-start-dir)
  (ignore-errors
	(delete-directory tdd-work-dir t))
  (mkdir tdd-work-dir)
  (cd tdd-work-dir)

  (cl-loop for idx in (file-expand-wildcards "../data/*") do
		   (if (file-directory-p idx) (copy-directory idx ".")
			 (copy-file idx "."))
		   ))

(defun pp-hash (hash)
  "Recursive describe hash func for nested hash-tables"
  (maphash (lambda (key value)
			 (pp key)
			 (princ " => ")
			 (if (hash-table-p value)
				 (progn
				   (princ " { ")
				   (terpri)
				   (describe-hash-descend value)
				   (princ " } "))
			   (pp value))
			 (terpri))
		   hash))

;; setup
(tdd-setup-global)
