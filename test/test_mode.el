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



(ert-run-tests-batch-and-exit (car argv))
