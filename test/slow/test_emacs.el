;; emacs source subdir with ~ 2830 files
(setq
 EMACS-LISP-DIR "/opt/tmp/emacs/lisp"
 CMD "find . -type f"
 )

(global-font-lock-mode 0)
(setq
 revert-without-query (quote (".*"))
 vc-handled-backends nil
 enable-local-variables nil)

(global-auto-revert-mode)

(ph-mode t)
(cd EMACS-LISP-DIR)
(ignore-errors
  (delete-file ".ph"))
(ph-project-new EMACS-LISP-DIR)

(setq files (split-string (shell-command-to-string CMD)))
(setq report (make-progress-reporter
			  "open file: " 0 (length files)))
(setq nFile 0)

(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk"
  )

(defun ask-user-about-lock (file opponent)
  "always grab lock"
  t)

(dolist (idx files)
  (cd EMACS-LISP-DIR)
  (find-file idx)
  (progress-reporter-update report (cl-incf nFile)))

(progress-reporter-done report)
