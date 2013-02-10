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

(defun ph-venture-opfl-get (pobj file)
  (gethash file (ph-ven-opfl pobj)))

(defun ph-venture-opfl-size (pobj)
  (hash-table-count (ph-ven-opfl pobj)))

(defun ph-venture-opfl-each (pobj blk)
  "Iterate through POBJ opfl hash"
  (maphash (lambda (key val)
			 (funcall blk key val))
		   (ph-ven-opfl pobj)))

(defun ph-venture-new (dir)
  "Create a new ph-ven in DIR & add it to ph-vl.
Return a pointer to a cell in ph-vl list or nil on error."
  (cl-block nil
	(unless dir (cl-return nil))

	(let (cell db)
	  (setq db (ph-db-get dir))
	  (when (setq cell (ph-vl-find db))
		(ph-warn 1 (format "project for %s is already loaded in emacs" dir))
		(cl-return cell))

	  (car (push (make-ph-ven :db db) ph-vl)))))



(defun ph-vl-reset ()
  "Empty ph-vl list."
  (setq ph-vl '()))

(defun ph-vl-size ()
  "Self explanatory."
  (length ph-vl))

(defun ph-vl-each (blk)
  "Iterate through ph-vl list"
  (cl-loop for idx in ph-vl do (funcall blk idx)))

(defun ph-vl-find (db)
  "Return a pointer to some ph-vl object or nil."
  (cl-block nil
	(unless db (cl-return nil))
	(ph-vl-each (lambda (pobj)
				  (if (equal db (ph-ven-db pobj)) (cl-return pobj))
				  ))
	))



(defun ph-db-get (dir)
  "Construct a proper db name from DIR."
  (if (not dir)
	  nil
	(concat (file-name-as-directory dir) ph-DB-NAME)))



(provide 'ph-venture)
