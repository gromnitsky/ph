:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ph)
(require 'tdd-helper)

(setq ph-verbose -1)



(ert-deftest ph-buffer-list()
  (ph-mode nil)
  (should-not (ph-buffer-list nil))

  (let (bf-a bf-b bf-c)
	(ph-project-open "a/.ph")
	(cd tdd-work-dir)
	(ph-project-open "b/.ph")
	(cd tdd-work-dir)
	(ph-project-open "level-1/.ph")
	(cd tdd-work-dir)

	(should (setq bf-a (ph-buffer-list (ph-vl-find "a/.ph"))))
	(should (setq bf-b (ph-buffer-list (ph-vl-find "b/.ph"))))
	(should (setq bf-c (ph-buffer-list (ph-vl-find "level-1/.ph"))))

	(should (equal 3 (length bf-a)))
	(should (equal 3 (length bf-b)))
	(should (equal 5 (length bf-c)))

	(ph-project-new "1/2/3")
	(should-not (ph-buffer-list (ph-vl-find "1/2/3/.ph")))

	(should (equal 3 (length bf-a)))

	;; cleanup
	(ph-project-close (ph-vl-find "a/.ph"))
	(ph-project-close (ph-vl-find "b/.ph"))
	(ph-project-close (ph-vl-find "level-1/.ph"))
	(ph-project-close (ph-vl-find "1/2/3/.ph"))
	(tdd-setup-global)
	))

(ert-deftest ph-buffer-pobj-get()
  (ph-mode nil)
  (should-not (ph-buffer-pobj-get))

  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (equal (expand-file-name "a/.ph")
				 (ph-ven-db (ph-buffer-pobj-get))))

  (ph-project-close (ph-vl-find "a/.ph"))
  (tdd-setup-global)
  )



(ert-deftest ph-project-open()
  (ph-mode nil)
  (should-not (ph-project-open nil))
  (should-not (ph-project-open "/DOESN'T EXIST"))

  (let (openedFiles pobj)
	;; a
	(setq openedFiles (ph-project-open "a/.ph"))
	(cd tdd-work-dir)
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
	(cd tdd-work-dir)
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

(ert-deftest ph-project-close()
  (ph-mode nil)
  (should-not (ph-project-close nil))

  ;; open 2 projects
  (should (ph-project-open "a/.ph"))
  (cd tdd-work-dir)
  (should (ph-project-open "b/.ph"))
  (cd tdd-work-dir)
  (should (equal 2 (ph-vl-size)))
  (should (equal 3 (length (ph-buffer-list (ph-vl-find "a/.ph")))))
  (should (equal 3 (length (ph-buffer-list (ph-vl-find "b/.ph")))))

  ;; close project a
  (should (ph-project-close (ph-vl-find "a/.ph")))
  (should (equal 1 (ph-vl-size)))
  (should (equal 0 (length (ph-buffer-list (ph-vl-find "a/.ph")))))
  (should (equal 3 (length (ph-buffer-list (ph-vl-find "b/.ph")))))

  ;; close project b
  (should (ph-project-close (ph-vl-find "b/.ph")))
  (should (equal 0 (ph-vl-size)))
  (should (equal 0 (length (ph-buffer-list (ph-vl-find "b/.ph")))))

;  (print (buffer-list))
  (tdd-setup-global)
  )

(ert-deftest ph-project-new_simple()
  (ph-mode nil)
  (should-not (ph-project-new nil))
  (should-error (ph-project-new "a"))	; project already exists

  (let (db1)
	(should (setq db1 (ph-project-new "project-new_simple/2/3")))
	(cd tdd-work-dir)
	(should (file-exists-p db1))
	(should (equal 1 (ph-vl-size)))

	(ph-project-close (ph-vl-find "project-new_simple/2/3/.ph"))
	(tdd-setup-global)
	))

(defun y-or-n-p(prompt)
  tdd-y-or-n)

(ert-deftest ph-project-new_subproject()
  (ph-mode nil)
  ;; open 1st project
  (should (= 4 (ph-project-open "level-1/.ph")))
  (cd tdd-work-dir)
  (should (equal 1 (ph-vl-size)))

  (let (db2)
	;; create a subproject
	(should (setq db2 (ph-project-new "level-1/level-2")))
	(cd tdd-work-dir)
	(should (equal 1 (ph-vl-size)))
	(should (ph-vl-find db2))

	;; 1st project is closed due to subproject; reopen it
	(should (= 2 (ph-project-open "level-1/.ph")))
	(should (equal 2 (ph-vl-size)))

	(ph-project-close (ph-vl-find "level-1/.ph"))
	(ph-project-close (ph-vl-find "level-1/level-2/.ph"))
	(tdd-setup-global)
	))

(ert-deftest ph-project-new_subproject_2()
  (ph-mode nil)
  ;; create a project
  (should (ph-project-new "."))

  ;; create a subproject
  (delete-file "level-1/.ph")
  (should (ph-project-new "level-1"))

  (ph-project-close (ph-vl-find "level-1/.ph"))
  (tdd-setup-global)
  )

(ert-deftest ph-project-new_subproject_fail()
  (ph-mode nil)
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

(ert-deftest ph-project-which()
  (ph-mode nil)
  (should-not (ph-project-which))

  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (equal (expand-file-name "a/.ph") (ph-project-which)))

  (ph-project-close "a/.ph")
  (tdd-setup-global)
  )

; monkey patch
(defun ido-completing-read (prompt
							choices
							&optional predicate require-match
							initial-input hist def inherit-input-method)
  (car choices))

(ert-deftest ph-project-switch-buffer()
  (ph-mode nil)
  (should-error (ph-project-switch-buffer nil))
  (should-error (ph-project-switch-buffer))

  (let (lst)
	(ph-project-open "level-1/.ph")
	(cd tdd-work-dir)

	;; 1st element in project buffer list must be 2nd
	;; after ph-project-switch-buffer
	(setq buried (car (ph-buffer-list (ph-vl-find "level-1/.ph"))))
	(should (ph-project-switch-buffer))
	(should (equal buried (nth 1 (ph-buffer-list (ph-vl-find "level-1/.ph")))))

	(ph-project-close (ph-vl-find "level-1/.ph"))
	(tdd-setup-global)
	))

(ert-deftest ph-project-switch()
  (ph-mode t)
  (should-error (ph-project-switch))

  (ph-project-open "a/.ph")
  (ph-project-open "b/.ph")
  (ph-project-open "level-1/.ph")

  (should (equal "a" (ph-project-switch)))
  (should (equal 'dired-mode
				 (buffer-local-value 'major-mode (get-buffer "a"))))

  (cd tdd-work-dir)
  (ph-project-close (ph-vl-find "a/.ph"))
  (ph-project-close (ph-vl-find "b/.ph"))
  ;; "level-1" dired buffer must be killed automatically
  (ph-project-close (ph-vl-find "level-1/.ph"))

  (tdd-setup-global)
  )

(ert-deftest ph-find-file-hook()
  (ph-mode t)

  (find-file "empty.txt")
  (should (equal 0 (ph-vl-size)))

  (find-file "b/b/c/memory")
  (should (equal 0 (ph-vl-size)))

  (cd tdd-work-dir)
  (should (equal 2 (ph-project-open "b/.ph")))
  (cd tdd-work-dir)
  (should (equal 1 (ph-vl-size)))
  (should (equal 2 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

	;; record another file
  (find-file "b/b/c/one.txt")
  (cd tdd-work-dir)
  (should (equal 3 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

  ;; parse raw db
  (should (equal 3 (ph-venture-opfl-size (ph-venture-unmarshalling "b/.ph"))))

  (find-file "/root/boots.txt")
  (cd tdd-work-dir)
  (should (equal 3 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

  (kill-buffer "empty.txt")
  (ph-project-close (ph-vl-find "b/.ph"))
  (tdd-setup-global)
  )

(ert-deftest ph-kill-file-hook()
  (ph-mode t)

  ;; check non-project file
  (find-file "empty.txt")
  (should (equal 0 (ph-vl-size)))
  (should (get-buffer "empty.txt"))
  (kill-buffer "empty.txt")
  (should-not (get-buffer "empty.txt"))

  ;; project a
  (should (equal 2 (ph-project-open "a/.ph")))
  (cd tdd-work-dir)
  (should (equal 1 (ph-vl-size)))
  (should (equal 2 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

  ;; kill buffer
  (kill-buffer "three.txt")
  (should-not (get-buffer "three.txt"))
  (should (equal 1 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

  ;; parse raw db
  (should (= 1 (ph-venture-opfl-size (ph-venture-unmarshalling "a/.ph"))))

  (ph-project-close (ph-vl-find "a/.ph"))
  (tdd-setup-global)
  )

(ert-deftest ph-kill-file-hook_permission_denied()
  (ph-mode t)

  ;; project a
  (should (equal 2 (ph-project-open "a/.ph")))
  (cd tdd-work-dir)
  (should (equal 1 (ph-vl-size)))
  (should (equal 2 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

  ;; fail to remove killed buffer from db
  (chmod "a/.ph" #o400)
  (kill-buffer "three.txt")
  (should-not (get-buffer "three.txt"))
  (should (equal 2 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

  ;; parse raw db
  (should (equal 2 (ph-venture-opfl-size (ph-venture-unmarshalling "a/.ph"))))

  (ph-project-close (ph-vl-find "a/.ph"))
  (tdd-setup-global)
  )

(ert-deftest ph-dired-after-readin-hook()
  (ph-mode t)

  ;; open some dir
  (find-file "/tmp")
  (should-not (local-variable-p 'ph-buffer-pobj (get-buffer "tmp")))

  ;; open project & its root dir
  (cd tdd-work-dir)
  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (find-file "a")
  (should (local-variable-p 'ph-buffer-pobj (get-buffer "a")))

  ;; open some non-project dir
  (find-file "/etc")
  (should-not (local-variable-p 'ph-buffer-pobj (get-buffer "etc")))

  ;; open some project dirs
  (cd tdd-work-dir)
  (mkdir "a/foo/bar" t)
  (find-file "a/foo")
  (should (local-variable-p 'ph-buffer-pobj (get-buffer "foo")))
  (should (local-variable-p 'ph-buffer-pobj (get-buffer "bar")))

  ;; open file in closed project
  (cd tdd-work-dir)
  (find-file "a/b/c")
  (should-not (local-variable-p 'ph-buffer-pobj (get-buffer "c")))

  ;; cleanup
  (cd tdd-work-dir)
  (ph-project-close (ph-vl-find "a/.ph"))
  (should-not (get-buffer "a"))
  (should-not (get-buffer "foo"))

  (kill-buffer "tmp")
  (kill-buffer "etc")
  (kill-buffer "c")
;  (print (buffer-list))
  (tdd-setup-global)
  )



(ert-run-tests-batch-and-exit (car argv))
