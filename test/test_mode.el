:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ph)
(require 'tdd-helper)

(setq ph-verbose -1)



(ert-deftest ph-project-open()
  (ph-vl-reset)

  (should-not (ph-project-open nil))
  (should-not (ph-project-open "/DOESN'T EXIST"))

  (let (openedFiles)
	;; a
	(setq openedFiles (ph-project-open "a/.ph"))
	(should (equal 2 openedFiles))
	(should (equal 1 (ph-vl-size)))

	;; open a again
	(setq openedFiles (ph-project-open "a/.ph"))
	(should-not openedFiles)
	(should (equal 1 (ph-vl-size)))

	;; b
	(setq openedFiles (ph-project-open "b/.ph"))
	(should (equal 2 openedFiles))
	(should (equal 2 (ph-vl-size)))

	;; open b again
	(setq openedFiles (ph-project-open "b/.ph"))
	(should-not openedFiles)
	(should (equal 2 (ph-vl-size)))

	(ph-vl-reset)
	))

(defun pobj-buffer-list (pobj)
  (cl-block nil
	(unless (ph-ven-p pobj) (cl-return nil))

	(let ((buli '())
		  cell)
	  (cl-loop for idx in (buffer-list) do
			   (if (and (buffer-file-name idx)
						(setq cell (buffer-local-value 'ph-buffer-pobj idx))
						(eq pobj (ph-vl-find (ph-ven-db cell))))
				   (push idx buli))
			   )
	  buli
	  )))

(ert-deftest ph-project-close()
  (ph-vl-reset)
  (should-not (ph-project-close nil))

  ;; open 2 projects
  (should (ph-project-open "a/.ph"))
  (should (ph-project-open "b/.ph"))
  (should (equal 2 (ph-vl-size)))
  (should (equal 2 (length (pobj-buffer-list (ph-vl-find "a/.ph")))))
  (should (equal 2 (length (pobj-buffer-list (ph-vl-find "b/.ph")))))

  ;; close project a
  (should (ph-project-close (ph-vl-find "a/.ph")))
  (should (equal 1 (ph-vl-size)))
  (should (equal 0 (length (pobj-buffer-list (ph-vl-find "a/.ph")))))
  (should (equal 2 (length (pobj-buffer-list (ph-vl-find "b/.ph")))))

  ;; close project b
  (should (ph-project-close (ph-vl-find "b/.ph")))
  (should (equal 0 (ph-vl-size)))
  (should (equal 0 (length (pobj-buffer-list (ph-vl-find "b/.ph")))))

;  (print (buffer-list))
  (ph-vl-reset)
  )

(ert-deftest ph-project-new_simple()
  (ph-vl-reset)
  (should-not (ph-project-new nil))
  (should-error (ph-project-new "a"))	; project already exists

  (let (db1)
	(should (setq db1 (ph-project-new "project-new_simple/2/3")))
	(should (file-exists-p db1))
	(should (equal 1 (ph-vl-size)))

	(tdd-setup-global)
	))



(ert-run-tests-batch-and-exit (car argv))
