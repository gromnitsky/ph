;; -*- lexical-binding: t -*-

(require 'cl-lib)

(setq
 vc-handled-backends nil
 argv (cdr argv))					; remove '--' from CL arguments

(setq ph-verbose 0)

(setq tdd-start-dir (file-name-directory load-file-name))
(setq tdd-work-dir (concat tdd-start-dir "tmp" ))
(message (format "wd == %s" tdd-work-dir))

(setq tdd-y-or-n t)

(defun tdd-setup-global ()
  (cd tdd-start-dir)
  (ignore-errors
	(delete-directory tdd-work-dir t))
  (mkdir tdd-work-dir)
  (zero-hook-counters)
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

(defun zero-hook-counters ()
  (setq ph-status-find-file-hook 0)
  (setq ph-status-kill-buffer-hook 0)
  (setq ph-status-dired-after-readin-hook 0)
  (setq ph-status-before-save-hook 0)
)

(defun hook-counters ()
  (format "%d %d %d %d"
		  ph-status-find-file-hook
		  ph-status-kill-buffer-hook
		  ph-status-dired-after-readin-hook
		  ph-status-before-save-hook
		  ))

(defun y-or-n-p(prompt)
  tdd-y-or-n)

;; setup
(tdd-setup-global)

(provide 'tdd-helper)
