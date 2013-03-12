:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ph)
(require 'tdd-helper)

(setq ph-verbose -1)

;; make reporters silent
(defun make-progress-reporter (message &optional min-value max-value
									   current-value min-change min-time)
  )
(defun progress-reporter-update (reporter &optional value)
  )
(defun progress-reporter-done (reporter)
  )

;; if tests runs only without those 2 lines, we have bugs in
;; ph-project-open or ph-project-close
(setq revert-without-query (quote (".*")))
(global-auto-revert-mode)

(ph-mode)



(ert-deftest 2hooks()
  (should (= 4 (ph-project-open "level-1/.ph")))
  (find-file "BWAA")
  (basic-save-buffer)

  (cd tdd-work-dir)
  (kill-buffer "w.txt")

  (should (ph-project-close (ph-vl-find "level-1/.ph")))
  (should (= 4 (ph-venture-opfl-size (ph-venture-unmarshalling "level-1/.ph"))))

  (should (equal "1 1 1 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-buffer-list()
  (should-not (ph-buffer-list nil))

  (let (bf-a bf-b bf-c)
	(ph-project-open "a/.ph")
	(cd tdd-work-dir)
	(ph-project-open "b/.ph")
	(cd tdd-work-dir)
	(ph-project-open "level-1/.ph")
	(cd tdd-work-dir)

	(should (setq bf-a (ph-buffer-list (ph-vl-find "a/.ph"))))
	(should (setq bf-b (ph-buffer-list (ph-vl-find "b/.ph") :names t)))
	(should (setq bf-c (ph-buffer-list (ph-vl-find "level-1/.ph"))))

	(should (equal 3 (length bf-a)))
	(dolist (idx bf-a)
	  (should (bufferp idx)))

	(should (equal 3 (length bf-b)))
	(dolist (idx bf-b)
	  (should (stringp idx)))

	(should (equal 5 (length bf-c)))

	(should (equal "level-1" (buffer-name (current-buffer))))
	(should (= 4 (length (ph-buffer-list (ph-vl-find "level-1/.ph") :nocb t))))

	(set-buffer "a")
	(should (= 5 (length (ph-buffer-list (ph-vl-find "level-1/.ph") :nocb t))))

	(ph-project-new "1/2/3")
	(cd tdd-work-dir)
	(should (= 1 (length (ph-buffer-list (ph-vl-find "1/2/3/.ph")))))

	(should (equal 3 (length bf-a)))

	;; cleanup
	(ph-project-close (ph-vl-find "a/.ph"))
	(ph-project-close (ph-vl-find "b/.ph"))
	(ph-project-close (ph-vl-find "level-1/.ph"))
	(ph-project-close (ph-vl-find "1/2/3/.ph"))

	(should (equal "0 0 4 0" (hook-counters)))
	(tdd-setup-global)
	))

(ert-deftest ph-buffer-pobj-get()
  (should-not (ph-buffer-pobj-get))

  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (equal (expand-file-name "a/.ph")
				 (ph-ven-db (ph-buffer-pobj-get))))

  ;; we are in dired buffer
  (should-not (local-variable-p 'ph-buffer-orig-file-name))

  (should (set-buffer "three.txt"))
  (should (local-variable-p 'ph-buffer-orig-file-name))
  (should (equal (buffer-file-name)
				 (expand-file-name ph-buffer-orig-file-name)))

  (ph-project-close (ph-vl-find "a/.ph"))

  (should (equal "0 0 1 0" (hook-counters)))
  (tdd-setup-global)
  )



(ert-deftest ph-project-open()
  (should-not (ph-project-open nil))
  (should-not (ph-project-open "/DOESN'T EXIST"))

  (let (openedFiles pobj)
	;; a
	(setq openedFiles (ph-project-open "a/.ph"))
	(cd tdd-work-dir)
	(should (equal 2 openedFiles))
	(should (equal 1 (ph-vl-size)))

	;; check that db was physically updated
	(setq pobj (read (ph-file-read "a/.ph")))
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
	(should (equal "0 0 2 0" (hook-counters)))
	(tdd-setup-global)
	))

(ert-deftest ph-project-close()
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
  (should (equal "0 0 2 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-project-new_simple()
  (should-not (ph-project-new nil))
  (should-error (ph-project-new "a"))	; project already exists

  (let (db1)
	(should (setq db1 (ph-project-new "project-new_simple/2/3")))
	(cd tdd-work-dir)
	(should (file-exists-p db1))
	(should (equal 1 (ph-vl-size)))

	(ph-project-close (ph-vl-find "project-new_simple/2/3/.ph"))
	(should (equal "0 0 1 0" (hook-counters)))
	(tdd-setup-global)
	))

(ert-deftest ph-project-new_subproject()
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
	(should (equal "0 0 3 0" (hook-counters)))
	(tdd-setup-global)
	))

(ert-deftest ph-project-new_subproject_2()
  ;; create a project
  (should (ph-project-new "."))
  (should (equal 1 (ph-vl-size)))

  ;; create a subproject
  (delete-file "level-1/.ph")
  (should (ph-project-new "level-1"))
  (should (equal 1 (ph-vl-size)))		; prev is closed as a subproject

  (ph-project-close (ph-vl-find "level-1/.ph"))
  (should (equal "0 0 2 0" (hook-counters)))
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

  (should (equal "0 0 0 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-project-new_files_already_opened()
  (ph-project-open "a/.ph")
  (should (= 1 (ph-vl-size)))

  (cd tdd-work-dir)
  (find-file "level-1/q.txt")
  (find-file "w.txt")
  (cd tdd-work-dir)
  (find-file "level-1/level-2/q.txt")
  (find-file "w.txt")

  (ph-project-new ".")
  (should (= 2 (ph-vl-size)))
  (cd tdd-work-dir)
  (should (= 2 (ph-venture-opfl-size (ph-vl-find "level-1/level-2/.ph"))))

  ;; db check
  (should (= 2 (ph-venture-opfl-size
				(ph-venture-unmarshalling "level-1/level-2/.ph"))))

  ;; cleanup
  (cd tdd-work-dir)
  (should (ph-project-close (ph-vl-find "level-1/level-2/.ph")))
  (ph-project-close (ph-vl-find "a/.ph"))
  (kill-buffer "q.txt")
  (kill-buffer "w.txt")
;  (print (buffer-list))
  (tdd-setup-global)
  )

(ert-deftest ph-project-which()
  (should-not (ph-project-which))

  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (equal (expand-file-name "a/.ph") (ph-project-which)))

  (ph-project-close (ph-vl-find "a/.ph"))
  (should (equal "0 0 1 0" (hook-counters)))
  (tdd-setup-global)
  )

; monkey patch
(defun ido-completing-read (prompt
							choices
							&optional predicate require-match
							initial-input hist def inherit-input-method)
  (car choices))

(ert-deftest ph-project-switch-buffer()
  (should-error (ph-project-switch-buffer nil))
  (should-error (ph-project-switch-buffer))

  (let (lst)
	(ph-project-open "level-1/.ph")
	(cd tdd-work-dir)

	;; 1st element in project buffer list must be 2nd
	;; after ph-project-switch-buffer
	(setq buried (car (ph-buffer-list (ph-vl-find "level-1/.ph"))))
	(should (ph-project-switch-buffer))
	(cd tdd-work-dir)
	(should (equal buried (nth 1 (ph-buffer-list (ph-vl-find "level-1/.ph")))))

	(ph-project-close (ph-vl-find "level-1/.ph"))
	(should (equal "0 0 1 0" (hook-counters)))
	(tdd-setup-global)
	))

(ert-deftest ph-project-switch()
  (should-error (ph-project-switch))

  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (ph-project-open "b/.ph")
  (cd tdd-work-dir)
  (ph-project-open "level-1/.ph")
  (cd tdd-work-dir)

  (should (equal "a" (ph-project-switch)))
  (should (equal 'dired-mode
				 (buffer-local-value 'major-mode (get-buffer "a"))))

  (ph-project-close (ph-vl-find "a/.ph"))
  (ph-project-close (ph-vl-find "b/.ph"))
  ;; "level-1" dired buffer must be killed automatically
  (ph-project-close (ph-vl-find "level-1/.ph"))

  (should (equal "0 0 3 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-find-file-hook()
  (find-file "empty.txt")
  (should (equal 0 (ph-vl-size)))

  (find-file "b/b/c/memory")
  (should (equal 0 (ph-vl-size)))

  (cd tdd-work-dir)
  (should (equal 2 (ph-project-open "b/.ph")))
  (cd tdd-work-dir)
  (should (equal 1 (ph-vl-size)))
  (should (equal 3 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

	;; record another file
  (find-file "b/b/c/one.txt")
  (cd tdd-work-dir)
  (should (equal 4 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

  ;; parse raw db
  (should (equal 4 (ph-venture-opfl-size (ph-venture-unmarshalling "b/.ph"))))

  (find-file "/root/boots.txt")
  (cd tdd-work-dir)
  (should (equal 4 (ph-venture-opfl-size (ph-vl-find "b/.ph"))))

  (kill-buffer "empty.txt")
  (ph-project-close (ph-vl-find "b/.ph"))

  (should (equal "1 0 1 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-kill-file-hook()
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
  (should (equal "0 1 1 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-kill-file-hook_permission_denied()
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
  (should (equal "0 1 1 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-dired-after-readin-hook()
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
  (should (equal "0 0 2 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-before-save-hook ()
  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (= 3 (length (ph-buffer-list (ph-vl-find "a/.ph")))))

  ;; move buffer
  (should (set-buffer "three.txt"))
  (rename-file "three.txt" "/tmp/NEW-THREE.TXT" t)
  (set-visited-file-name "/tmp/NEW-THREE.TXT")
  (basic-save-buffer)

  ;; moved buffer must be deleted from ph-vl cell & unmarked
  (cd tdd-work-dir)
  (should-not (ph-buffer-pobj-get))
  (should (= 2 (length (ph-buffer-list (ph-vl-find "a/.ph")))))
  (should (= 1 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

  ;; rename buffer but keep it under project directory
  (should (set-buffer "two.txt"))
  (rename-file "two.txt" "../TWO.TXT" t)
  (set-visited-file-name "../TWO.TXT")
  (basic-save-buffer)

  (cd tdd-work-dir)
  (should (ph-buffer-pobj-get))
  (should (= 2 (length (ph-buffer-list (ph-vl-find "a/.ph")))))
  (should (= 1 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))
  (should (ph-venture-opfl-get (ph-vl-find "a/.ph")
							   ph-buffer-orig-file-name))

  (delete-file "/tmp/NEW-THREE.TXT")
  (ph-project-close (ph-vl-find "a/.ph"))

  ;; reopen project
  (ph-project-open "a/.ph")
  (cd tdd-work-dir)
  (should (= 2 (length (ph-buffer-list (ph-vl-find "a/.ph")))))
  (ph-project-close (ph-vl-find "a/.ph"))

  (should (equal "0 0 2 2" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-before-save-hook_readonly()
  (chmod "a" #o500)
  (unwind-protect
	  (progn
		(ph-project-open "a/.ph")

		;; move buffer
		(should (set-buffer "three.txt"))
		(set-visited-file-name "/tmp/NEW-THREE.TXT")
		(basic-save-buffer)

		;; close
		(cd tdd-work-dir)
		(ph-project-close (ph-vl-find "a/.ph"))

		;; reopen
		(ph-project-open "a/.ph")
		(cd tdd-work-dir)
		(should (= 2 (ph-venture-opfl-size (ph-vl-find "a/.ph"))))

		(ph-project-close (ph-vl-find "a/.ph"))
		(delete-file "/tmp/NEW-THREE.TXT"))
	(chmod "a" #o755)
	(should (equal "0 0 2 1" (hook-counters)))
	(tdd-setup-global))
  )

(ert-deftest ph-before-save-hook_prefix()
  (ph-project-open "level-1/.ph")
  (cd tdd-work-dir)
  (should (= 5 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  (should (set-buffer "w.txt<2>"))
  (insert "hi")
  (basic-save-buffer)

  (cd tdd-work-dir)
  (should (= 5 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  (cd tdd-work-dir)
  (ph-project-close (ph-vl-find "level-1/.ph"))
  (should (equal "0 0 1 0" (hook-counters)))
  (tdd-setup-global)
  )

(ert-deftest ph-project-file-mv()
  (ph-project-open "level-1/.ph")
  (cd tdd-work-dir)
  (should (= 5 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  ;; move to tmp
  (should (set-buffer "w.txt"))
  (ph-project-file-mv "/tmp/NEW-W.TXT")

  (cd tdd-work-dir)
  (should (= 4 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  ;; move to subdir
  (should (set-buffer "q.txt<2>"))
  (cd "..")
  (ph-project-file-mv "1/2/3/")

  (cd tdd-work-dir)
  (should (= 4 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  ;; move it mo parent dir
  (cd "level-1")
  (ph-project-file-mv "1/2")
  (cd tdd-work-dir)
  (should (= 4 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  (cd tdd-work-dir)
  (ph-project-close (ph-vl-find "level-1/.ph"))
  (should (equal "0 0 1 3" (hook-counters)))
  (delete-file "/tmp/NEW-W.TXT")
  (tdd-setup-global)
  )

(ert-deftest ph-project-file-mv_fail()
  (should (set-buffer "*scratch*"))
  (should-error (ph-project-file-mv "/tmp/NEW-W.TXT"))

  (ph-project-open "level-1/.ph")
  (cd tdd-work-dir)
;  (should (= 5 (length (ph-buffer-list (ph-vl-find "level-1/.ph")))))

  (should (set-buffer "w.txt"))
  (should-error (ph-project-file-mv "/root"))
  (should-error (ph-project-file-mv nil))
  (should-error (ph-project-file-mv ""))
  (should-error (ph-project-file-mv (expand-file-name "w.txt")))

  (make-symbolic-link "w.txt" "symlink-w.txt")
  (kill-buffer "w.txt")
  (cd (concat tdd-work-dir "/level-1"))
  (find-file "symlink-w.txt")
  (should-error (ph-project-file-mv "w.txt"))

  (cd tdd-work-dir)
  (ph-project-close (ph-vl-find "level-1/.ph"))
  (should (equal "1 1 1 0" (hook-counters)))
  (tdd-setup-global)
  )




(ert-run-tests-batch-and-exit (car argv))
