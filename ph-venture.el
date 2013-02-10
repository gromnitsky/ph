;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)

;(require 'ph-u)

(defconst ph-DB-NAME ".ph" "A physicall db file (json)")
(defvar ph-vl '() "Global list of currently opened projects")

;; See doc/structure.org for desc.
(cl-defstruct ph-ven
  "Phoject Helper object"
  db
  (version ph-meta-version)
  (opfl (make-hash-table :test 'equal))
)



(defun ph-venture-new (dir)
  "Create a new ph-ven in dir & add it to ph-vl.
Return a pointer to ph-vl or nil on error."
  (cl-block nil
	(unless dir (error "DIR in nil"))
	; FIXME: write it
	))

(defun ph-venture-name (pobj)
  "Extract project name from POBJ. Return nil on error."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(let ((name (file-name-nondirectory (ph-dirname (ph-ven-db pobj)))))
	  (if (equal "" name)
		  "Root"
		name))))

(defun ph-venture-opfl-add (pobj file)
  (puthash file (float-time) (ph-ven-opfl pobj)))

(defun ph-venture-opfl-rm (pobj file)
  (remhash file (ph-ven-opfl pobj)))

(defun ph-venture-opfl-each (pobj blk)
  "Iterate through POBJ opfl hash"
  (maphash (lambda (key val)
			 (funcall blk key val))
		   (ph-ven-opfl pobj)))



(defun ph-vl-reset ()
  "Empty ph-vl list."
  (setq ph-vl '()))

(defun ph-vl-each (blk)
  "Iterate through ph-vl list"
  (cl-loop for idx in ph-vl do (funcall blk idx)))



(provide 'ph-venture)
