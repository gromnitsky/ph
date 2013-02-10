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

(ert-deftest ph-venture-new()
  (let (p1 p2 p21)
	(ph-vl-reset)
	(should (= 0 (ph-vl-size)))

	(should-not (ph-venture-new nil))

	(setq p1 (ph-venture-new "/foo"))
	(should (= 1 (ph-vl-size)))

	;; try to create the same object
	(setq p2 (ph-venture-new "/foo"))
	(should (= 1 (ph-vl-size)))

	;; find & compare
	(setq p21 (ph-vl-find (ph-db-get "/foo")))
	(should (equal p2 p21))

	(ph-vl-reset)
  ))

(ert-deftest ph-venture-marshalling()
  (let (p1 p2)
	(setq p1 (make-ph-ven :db ".ph"))
	(ph-venture-opfl-add p1 "one")
	(ph-venture-opfl-add p1 "two")

	(should-not (ph-venture-marshalling nil))
	(should (ph-venture-marshalling p1))

	;; read from the disk & compare
	(setq p2 (ph-venture-unmarshalling ".ph"))
	(should (ph-ven-p p2))
	(should (equal (ph-ven-db p1) (ph-ven-db p2)))
	(should (equal (ph-ven-version p1) (ph-ven-version p2)))
	(should (equal (ph-venture-opfl-get p1 "one") (ph-venture-opfl-get p2 "one")))
	(should (equal (ph-venture-opfl-get p1 "two") (ph-venture-opfl-get p2 "two")))

	(ph-venture-opfl-add p2 "three")
	(should (ph-venture-marshalling p2))

	(tdd-setup-global)
  ))



(ert-deftest ph-vl-find()
  (let ((p1 (make-ph-ven :db "/foo/bar/.ph"))
		(p2 (make-ph-ven :db "/foobar/.ph"))
		(p3 (make-ph-ven :db "/.ph"))
		p21
		p22)

	(push p1 ph-vl)
	(push p2 ph-vl)
	(push p3 ph-vl)

	(should-not (ph-vl-find nil))
	(should-not (ph-vl-find "BOGUS"))
	(should (equal p2 (ph-vl-find "/foobar/.ph")))

	;; check that the return value is a pointer, not a copied object
	(setq p21 (ph-vl-find "/foobar/.ph"))
	(ph-venture-opfl-add p21 "one")
	(should (= 1 (ph-venture-opfl-size p21)))

	(setq p22 (ph-vl-find "/foobar/.ph"))
	(should (> (ph-venture-opfl-get p22 "one") 100))
	(ph-venture-opfl-rm p22 "one")

	(should (= 0 (ph-venture-opfl-size p21)))

	(ph-vl-reset)
  ))

(ert-run-tests-batch-and-exit (car argv))
