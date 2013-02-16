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

(defun ph-project-parse (file)
  "Load a project form FILE as db and return the project object.
If the project already loaded, just return a pointer to ph-vl list.
Doesn't load any opfl files.

Return nil on error."
  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(let (pobj)
	  (if (setq pobj (ph-vl-find file)) (cl-return pobj))

	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 0 (format "cannot parse project %s" file))
		(cl-return nil))

	  (ph-vl-add pobj)
	  )))

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

	  (setq saveDir default-directory)
	  (setq cell (ph-vl-add pobj))
	  (remove-hook 'find-file-hook 'ph-find-file-hook)
	  (unwind-protect
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
												  (error-message-string err))))
									(ph-venture-opfl-rm pobj key))

								  (cd saveDir)))
		;; always restore the hook
		(add-hook 'find-file-hook 'ph-find-file-hook))

	  openedFiles)))

(defun ph-project-close (&optional pobj)
  "Close all currently opened project files. Return t on success."
  (interactive)
  (cl-block nil
	(unless (ph-ven-p pobj)
	  (progn
		;; try to get pobj from current buffer
		(if (and (local-variable-p 'ph-buffer-pobj)
				 (ph-ven-p ph-buffer-pobj))
			(setq pobj ph-buffer-pobj)
		  (progn
			(ph-warn 0 (format "%s doesn't belong to any project"
							   (current-buffer)))
			(cl-return nil)))
		))

	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	;; kill buffers in usual emacs fashion, some buffers may be unsaved
	;; & user can press C-g thus killing only a subset of buffers
	(unwind-protect
		(ph-venture-opfl-each pobj (lambda (key val)
									 (let (bufname)
									   (ignore-errors
										 (setq bufname
											   (get-file-buffer
												(ph-venture-opfl-absolute pobj key)))

										 (if bufname (kill-buffer bufname))
										 ))))
	  ;; always restore the hook
	  (add-hook 'kill-buffer-hook 'ph-kill-buffer-hook))

	;; remove project from ph-vl if user didn't hit C-g
	(ph-vl-rm (ph-ven-db pobj))
	t))

(defun ph-project-new (dir)
  "Create a new project in DIR. If DIR doens't exist it will be created.
Return a path to db.  If DIR is a subproject, close parent
project & clean its db from subproject files."
  (interactive "GCreate project in: ")
  (cl-block nil
	(if (not dir) (cl-return nil))
	(let ((db (ph-db-get dir))
		  parDb parObj)
	  (if (file-exists-p db)
		  (error "There is already a project in %s" dir))

	  (when (setq parDb (ph-db-find-subproject dir))
		  (if (not (y-or-n-p (format "Directory %s is alredy under project %s. \
Make a sub-project?" dir parDb)))
			  (cl-return nil)
			;; Close a sub project & fix its db.  Of cource it's better
			;; to "transfer" opfl subproject's files to a new project
			;; in real time, but that's too much work & emacs is an old fart.
			(when (not (setq parObj (ph-project-parse parDb)))
			  (error "Parsing sub-project %s failed. \
New project was NOT created" parDb))
			(ph-project-close parObj)
			(ph-venture-clean parObj (ph-file-relative dir (ph-dirname parDb)))
			(when (not (ph-venture-marshalling parObj))
			  (error "Updating sub-project %s failed. \
New project was NOT created" parDb))
			))
	  (if (not (file-directory-p dir)) (mkdir dir t))

	  (unless (and (ph-venture-marshalling (ph-venture-new db))
				   (ph-vcs-init dir))
		(error "Cannot create project in %s" dir))

	  db
	  )))



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
