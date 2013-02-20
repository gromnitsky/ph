;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ido)

(require 'ph-venture)



(defun ph-find-file-hook()
  (cl-block nil
	(let (db pobj file)
	  (if (not buffer-file-name) (cl-return))
	  (unless (setq db (ph-db-find buffer-file-name)) (cl-return nil))

	  (when (setq pobj (ph-vl-find db))
		(setq file (ph-file-relative buffer-file-name (ph-dirname db)))

		(ph-buffer-pobj-set pobj)
		(ph-venture-opfl-add pobj file)
		(ph-venture-marshalling pobj))
	  )))

(defun ph-kill-buffer-hook()
  (cl-block nil
	(if (or (not buffer-file-name)
			(not (ph-buffer-pobj-get)))
			(cl-return))

	(let (pobj file)
	  (setq pobj (ph-buffer-pobj-get))
	  (setq file (ph-file-relative buffer-file-name
								   (ph-dirname (ph-ven-db pobj))))

	  (ph-venture-opfl-rm pobj file)
	  (unless (ph-venture-marshalling pobj)
		;; restore file in ph-vl POBJ if marshalling failed
		(ph-venture-opfl-add pobj file))
	  )))

(defun ph-dired-after-readin-hook ()
  "Marks buffer as belonging to project if dired dir is a child to project dir."
  (cl-block nil
	(let (pobj cwd db)
	  (unless (stringp dired-directory) (cl-return))
	  (setq cwd (directory-file-name (expand-file-name dired-directory)))

	  (unless (setq db (ph-db-find cwd)) (cl-return))

	  (if (setq pobj (ph-vl-find db)) (ph-buffer-pobj-set pobj))
	  )))



(defun ph-buffer-pobj-get (&optional buf)
  (unless (bufferp buf) (setq buf (current-buffer)))

  (if (and (local-variable-p 'ph-buffer-pobj buf)
		   (ph-ven-p (buffer-local-value 'ph-buffer-pobj buf)))
	  (buffer-local-value 'ph-buffer-pobj buf)))

(defun ph-buffer-pobj-set (pobj)
  (if (ph-ven-p pobj)
	  (setq-local ph-buffer-pobj pobj)
	))

(defun ph-buffer-pobj-unset ()
  (ignore-errors
	(makunbound ph-buffer-pobj)))

(defun ph-buffer-list (pobj)
  "Iterate through buffer-list & return only POBJ buffers."
  (cl-block nil
	(let ((flist '()) cell)
	  (unless (ph-ven-p pobj) (cl-return flist))

	  (dolist (idx (buffer-list))
		(if (and (setq cell (ph-buffer-pobj-get idx))
				 (eq pobj cell))
			(setq flist (append flist (list idx)))))
	  flist)))



(defun ph-project-which (&optional pobj)
  "Print a path to a project db for current buffer.
Return pobj db or nil on error."
  (interactive)
  (cl-block nil
	(when (and (not (ph-ven-p pobj))
			   (not (setq pobj (ph-buffer-pobj-get))))
	  (ph-warn 0 (format "%s doesn't belong to any opened project" (current-buffer)))
	  (cl-return nil))

	(ph-warn 0 (format "%s: %s" (ph-venture-name pobj) (ph-ven-db pobj)))
	(ph-ven-db pobj)
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

;; 0) Parses FILE.
;; 1) Adds the project to ph-vl list.
;; 2) For each file in a project a) opens it, b) points buffer local
;;    variable to the project object.
(defun ph-project-open (file)
  "Return a number of opened files or nil on error."
  (interactive "fOpen .ph file: ")
  (cl-block nil
	(if (or (not file) (not (stringp file))) (cl-return nil))

	(let ((openedFiles 0) (wasFiles 0)
		  pobj pfile saveDir cell)
	  (when (not (setq pobj (ph-venture-unmarshalling file)))
		(ph-warn 0 (format "cannot parse project %s" file))
		(cl-return nil))
	  (when (ph-vl-find file)
		(ph-warn 1 (format "project %s is already loaded in emacs" file))
		(cl-return nil))

	  (setq wasFiles (ph-venture-opfl-size pobj))
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

	  ;; sync db with memory objects
	  (if (/= openedFiles wasFiles)
		  (ph-venture-marshalling pobj))

	  openedFiles)))

(defun ph-project-close (&optional pobj)
  "Close all currently opened project files. Return t on success."
  (interactive)
  (cl-block nil
  (when (and (not (ph-ven-p pobj))
			 (not (setq pobj (ph-buffer-pobj-get))))
	(ph-warn 0 (format "%s doesn't belong to any opened project" (current-buffer)))
	(cl-return nil))

	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	;; kill buffers in usual emacs fashion, some buffers may be unsaved
	;; & user can press C-g thus killing only a subset of buffers
	(unwind-protect
		(dolist (idx (ph-buffer-list pobj))
		  (with-demoted-errors
			(if idx (kill-buffer idx))))
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

(defun ph-project-switch-buffer (&optional pobj)
  "Like ido-swithc-buffer but only for a specific POBJ.
Return a buffer name if switch was done."
  (interactive)
  (if (and (not (ph-ven-p pobj))
		   (not (setq pobj (ph-buffer-pobj-get))))
	  (error "%s doesn't belong to any opened project" (current-buffer)))

  (let ((flist '())
		bf buf)
	(if (not (setq bf (ph-buffer-list pobj)))
		(error "Project %s doesn't have opened files yet" (ph-ven-db pobj)))

	;; create a list of POBJ emacs buffer names (not file names)
	(dolist (idx bf)
	  (setq flist (append flist (list (buffer-name idx))))) ; elisp is boring

	(when (setq buf (ido-completing-read "ph: " flist))
	  (switch-to-buffer buf))

	buf
	))

(defun ph-project-switch ()
  "Switch to a root directory of a selected opened project.
Unlike ph-project-switch-buffer it doesn't consider previous user's choices.

Return selected project name."
  (interactive)
  (if (= 0 (ph-vl-size))
	  (error "No opened projects yet. Type ph-project-open or ph-project-new"))

  (let (choice)
	(when (setq choice (ido-completing-read "Project: " (ph-vl-names)))
	  (find-file (ph-venture-opfl-prefix (ph-vl-find-by-name choice))))

	choice
	))



(define-minor-mode ph-mode
  "Toggle global minor Project Helper files tracking mode."
  :lighter " ph"
  :global t
  (if ph-mode
	  (progn
		(add-hook 'find-file-hook 'ph-find-file-hook)
		(add-hook 'dired-after-readin-hook 'ph-dired-after-readin-hook)
		(add-hook 'kill-buffer-hook 'ph-kill-buffer-hook))
	(remove-hook 'find-file-hook 'ph-find-file-hook)
	(remove-hook 'dired-after-readin-hook 'ph-dired-after-readin-hook)
	(remove-hook 'kill-buffer-hook 'ph-kill-buffer-hook)
	))

(provide 'ph)
