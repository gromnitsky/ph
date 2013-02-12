;; -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'ph-venture)



(defun ph-find-file-hook()
  )

(defun ph-kill-buffer-hook()
  )

(defun ph-project-open (file)
  "Return a number of opened files or nil on error."
  (interactive "fOpen .ph file: ")

  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(let ((openedFiles 0)
		  cell pobj pfile saveDir)
	  (when (ph-vl-find file)
		(ph-warn 1 (format "project %s is already loaded in emacs" file))
		(cl-return nil))

	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 0 (format "cannot parse project %s" file))
		(cl-return nil))

	  (remove-hook 'find-file-hook 'ph-find-file-hook)
	  (setq saveDir default-directory)
	  (ph-venture-opfl-each pobj
							(lambda (key val)
							  (setq pfile (ph-venture-opfl-absolute pobj key))
;							  (print (format "%s %s %s"
;											 default-directory pfile
;											 (file-readable-p pfile)))
							  (if (file-readable-p pfile)
								  (condition-case err
									  (progn
										(find-file pfile)
										(cl-incf openedFiles)
										)
									(error
									 (ph-venture-opfl-rm pobj key)
									 (ph-warn 0 "find-file failed: %s"
											  (cadr err))))
								(ph-venture-opfl-rm pobj key))

							  (cd saveDir)
							  ))
	  (push pobj ph-vl)
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
									(ph-venture-opfl-absolute pobj key)
									))))
	;; remove project from ph-vl
	(ph-vl-rm (ph-ven-db db))
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
