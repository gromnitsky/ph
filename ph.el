;; -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'ph-venture)



(defun ph-find-file-hook()
  )

(defun ph-kill-buffer-hook()
  )

(defun ph-buffer-pobj-set (pobj)
  (if (and buffer-file-name (ph-ven-p pobj))
	  (setq-local ph-buffer-pobj pobj)
	))

(defun ph-buffer-pobj-unset ()
  (if buffer-file-name
	  (ignore-errors
		(makunbound ph-buffer-pobj))
	))

;; What it does:
;;
;; 0) Parses FILE.
;;
;; 1) Adds the project to ph-vl list.
;;
;; 2) For each file in a project a) opens it, b) points buffer local
;; variable to the project object.
(defun ph-project-open (file)
  "Return a number of opened files or nil on error."
  (interactive "fOpen .ph file: ")
  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(let ((openedFiles 0)
		  pobj pfile saveDir cell)
	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 0 (format "cannot parse project %s" file))
		(cl-return nil))
	  (when (ph-vl-find file)
		(ph-warn 1 (format "project %s is already loaded in emacs" file))
		(cl-return nil))

	  (remove-hook 'find-file-hook 'ph-find-file-hook)
	  (setq saveDir default-directory)
	  (setq cell (ph-vl-add pobj))
	  (ph-venture-opfl-each pobj
							(lambda (key val)
							  (setq pfile (ph-venture-opfl-absolute pobj key))
							  (if (file-readable-p pfile)
								  (condition-case err
									  (progn
										(find-file pfile)
										(ph-buffer-pobj-set cell)
										(cl-incf openedFiles)
										)
									(error
									 (ph-venture-opfl-rm pobj key)
									 (ph-warn 0 "find-file failed: %s"
											  (cadr err))))
								(ph-venture-opfl-rm pobj key))

							  (cd saveDir)
							  ))
	  (add-hook 'find-file-hook 'ph-find-file-hook)
	  openedFiles)))

;; FIXME: test it
(defun ph-project-close (pobj)
  "Close all currently opened project files. Return t on success."
  (cl-block nil
	(unless pobj (cl-return nil))

	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	;; kill buffers in usual emacs fashion
	(ph-venture-opfl-each pobj (lambda (key val)
								 (ignore-errors
								   (kill-buffer
									(get-file-buffer
									 (ph-venture-opfl-absolute pobj key))
									))))
	;; remove project from ph-vl
	(ph-vl-rm (ph-ven-db pobj))
	(add-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	t))



(define-minor-mode ph-mode
  "Toggle global minor Project Helper files tracking mode."
  :lighter " ph"
  :global t
  (if ph-mode
	  (progn
		(add-hook 'find-file-hook 'ph-find-file-hook)
		(add-hook 'kill-buffer-hook 'ph-kill-buffer-hook))
	(remove-hook 'find-file-hook 'ph-find-file-hook)
	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	(ph-pl-reset)
	))

(provide 'ph)
