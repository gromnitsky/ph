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

  (let (openedFiles pobj)
	;; a
	(setq openedFiles (ph-project-open "a/.ph"))
	(should (equal 2 openedFiles))
	(should (equal 1 (ph-vl-size)))

	;; check that db was physically updated
	(setq pobj (read (ph-read-file "a/.ph")))
	(should (equal 2 (ph-venture-opfl-size pobj)))

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

	;; cleanup
	(ph-project-close (ph-vl-find "a/.ph"))
	(ph-project-close (ph-vl-find "b/.ph"))
	(tdd-setup-global)
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
  (tdd-setup-global)
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

(defun y-or-n-p(prompt)
  tdd-y-or-n)

(ert-deftest ph-project-new_subproject()
  (ph-vl-reset)

  ;; open 1st project
  (should (= 4 (ph-project-open "level-1/.ph")))
  (should (equal 1 (ph-vl-size)))

  (let (db2)
	;; create a subproject
	(should (setq db2 (ph-project-new "level-1/level-2")))
	(should (equal 1 (ph-vl-size)))
	(should (ph-vl-find db2))

	;; 1st project is closed due to subproject; reopen it
	(should (= 2 (ph-project-open "level-1/.ph")))
	(should (equal 2 (ph-vl-size)))

	(tdd-setup-global)
	))

(ert-deftest ph-project-new_subproject_2()
  (ph-vl-reset)

  ;; create a project
  (should (ph-project-new "."))

  ;; create a subproject
  (delete-file "level-1/.ph")
  (should (ph-project-new "level-1"))

  (tdd-setup-global)
  )

(ert-deftest ph-project-new_subproject_fail()
  (let ((tdd-y-or-n nil))
	(should-not (ph-project-new "level-1/level-2"))
	)

  (chmod "level-1/.ph" #o400)
  ;; permission denied updating project in level-1
  (should-error (ph-project-new "level-1/level-2"))
  (should-error (ph-project-new "level-1/level-2"))

  (chmod "level-1/.ph" #o644)
  (with-temp-file "level-1/.ph" (insert "bwaa!"))
  ;; level-1 project parsing error
  (should-error (ph-project-new "level-1/level-2"))

  (tdd-setup-global)
  )

(ert-deftest ph-buffer-current-pobj-get()
  (should-not (ph-buffer-current-pobj-get))
  (ph-project-open "a/.ph")
  (should (equal "a/.ph" (ph-ven-db (ph-buffer-current-pobj-get))))

  (ph-project-close "a/.ph")
  (tdd-setup-global)
  )

(ert-deftest ph-project-which()
  (should-not (ph-project-which))

  (ph-project-open "a/.ph")
  (should (equal "a/.ph" (ph-project-which)))

  (ph-project-close "a/.ph")
  (tdd-setup-global)
  )



(ert-run-tests-batch-and-exit (car argv))
