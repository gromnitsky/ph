:; exec emacs -Q --script "$0" -- "$@" # -*-emacs-lisp-*-

(load (expand-file-name "../ph-u.el") nil t)
(load (expand-file-name "../ph-venture.el") nil t)
(load (expand-file-name "./helper.el") nil t)



(ert-deftest ph-venture-name()
  (should (equal "bar" (ph-venture-name (make-ph-ven :db "/foo/bar/.ph"))))
  (should (equal "Root" (ph-venture-name (make-ph-ven :db "/.ph"))))

  (should-not (ph-venture-name nil))
  )

(ert-deftest ph-venture-opfl-each()
  (let (p1 list1)
	(setq p1 (make-ph-ven :db "/.ph"))
	(ph-venture-opfl-add p1 "one")
	(ph-venture-opfl-add p1 "two")
	(ph-venture-opfl-add p1 "three")

	(ph-venture-opfl-each p1 (lambda (key val)
							   (push key list1)
							   ))
	(should (equal '("three" "two" "one") list1)) ; not portable
	))



(ert-run-tests-batch-and-exit (car argv))
