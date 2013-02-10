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

(defun ph-venture-new (db)
  "Create a new ph-ven with DB as db & add it to ph-vl.
Return a pointer to a cell in ph-vl list or nil on error.

Doesn't do any I/O."
  (cl-block nil
	(if (or (not db) (not (stringp db))) (cl-return nil))

	(let (cell)
	  (when (setq cell (ph-vl-find db))
		(ph-warn 1 (format "project %s is already loaded in emacs" db))
		(cl-return cell))

	  (car (push (make-ph-ven :db db) ph-vl)))))

(defun ph-venture-marshalling (pobj)
  "Update POBJ in a marshalled form. Return t on success, nil otherwise.
WARNING: it rewrites the file every time."
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(condition-case err
		;; most typical error would be "permission denied" if user has
		;; opened a project on a read-only partition
		(with-temp-file (ph-ven-db pobj)
		  (insert (prin1-to-string pobj)))
	  (error (cl-return nil)))
	t
	))

(defun ph-venture-unmarshalling (file)
  "Return parsed ph-ven object from FILE or nil on error."
  (cl-block nil
	(unless file (cl-return nil))

	(let (pobj)
	  (condition-case err
		  (setq pobj
				(read (with-current-buffer
						  (find-file-noselect file) (buffer-string))))
		(error (cl-return nil)))
	  (unless (ph-ven-p pobj) (cl-return nil))

	  pobj
	  )))

(defun ph-venture-clean (pobj dir)
  (error "write me")
  )



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

(defun ph-db-new (file)
  (error "write me")
  )

(defun ph-db-find (dir)
  (error "write me")
  )



(provide 'ph-venture)
