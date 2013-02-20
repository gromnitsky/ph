:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ph-venture)
(require 'tdd-helper)



(ert-deftest ph-venture-name-simple()
  (should (equal "bar" (ph-venture-name-simple (make-ph-ven :db "/foo/bar/.ph"))))
  (should (equal "Root" (ph-venture-name-simple (make-ph-ven :db "/.ph"))))

  (should-not (ph-venture-name-simple nil))
)

(ert-deftest ph-venture-name()
  (should-not (ph-venture-name nil))
  (should-not (ph-venture-name (make-ph-ven :db "/foo/bar/.ph")))

  (let (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11)
	(ph-vl-add (setq p1 (make-ph-ven :db "/a/bar/.ph")))
	(ph-vl-add (setq p2 (make-ph-ven :db "/b/x/foo/.ph")))
	(ph-vl-add (setq p3 (make-ph-ven :db "/c/bar/.ph")))
	(ph-vl-add (setq p4 (make-ph-ven :db "/d/xx/foo/.ph")))
	(ph-vl-add (setq p5 (make-ph-ven :db "/e/foobar/.ph")))
	(ph-vl-add (setq p6 (make-ph-ven :db "/f/foobar/.ph")))
	(ph-vl-add (setq p7 (make-ph-ven :db "/g/baz/.ph")))
	(ph-vl-add (setq p8 (make-ph-ven :db "/.ph")))
	(ph-vl-add (setq p9 (make-ph-ven :db "/h/bar/.ph")))

	(should (equal "bar" (ph-venture-name p1)))
	(should (equal "foo" (ph-venture-name p2)))
	(should (equal "bar<2>" (ph-venture-name p3)))
	(should (equal "foo<2>" (ph-venture-name p4)))
	(should (equal "foobar" (ph-venture-name p5)))
	(should (equal "foobar<2>" (ph-venture-name p6)))
	(should (equal "baz" (ph-venture-name p7)))
	(should (equal "Root" (ph-venture-name p8)))
	(should (equal "bar<3>" (ph-venture-name p9)))

	;; add another
	(ph-vl-add (setq p10 (make-ph-ven :db "/i/bar/.ph")))
	(should (equal "bar<4>" (ph-venture-name p10)))

	;; broke limit
	(let ((ph-SIM-DIR-MAX 3))
	  (ph-vl-add (setq p11 (make-ph-ven :db "/j/bar/.ph")))
	  (should-error (ph-venture-name p11)))

	;; remove some projects
	(ph-vl-rm (ph-ven-db p3))
	(should (equal "bar<3>" (ph-venture-name p10)))
	(should (equal "bar<4>" (ph-venture-name p11)))

	(ph-vl-reset)
	))

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
	(should (= 0 (ph-vl-size)))

	(should-not (ph-venture-new nil))
	(should-not (ph-venture-new 1))

	(setq p1 (ph-venture-new (ph-db-get "/foo")))
	(should (= 1 (ph-vl-size)))

	;; try to create the same object
	(setq p2 (ph-venture-new (ph-db-get "/foo")))
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
	(should (equal (concat default-directory ".ph")
				   (ph-ven-db p2))) ; after a fix
	(should (ph-ven-p p2))
	(should (equal (ph-ven-version p1) (ph-ven-version p2)))
	(should (equal (ph-venture-opfl-get p1 "one") (ph-venture-opfl-get p2 "one")))
	(should (equal (ph-venture-opfl-get p1 "two") (ph-venture-opfl-get p2 "two")))

	(ph-venture-opfl-add p2 "three")
	(should (ph-venture-marshalling p2))

	(tdd-setup-global)
  ))

(ert-deftest ph-venture-clean()
  (let (p1 waste)
	(setq p1 (make-ph-ven :db "/q/w/e/foo/.ph"))
	(ph-venture-opfl-add p1 "bar/one.txt")
	(ph-venture-opfl-add p1 "two.txt")
	(ph-venture-opfl-add p1 "baz/three.txt")
	(ph-venture-opfl-add p1 "baz/four.txt")
	(ph-venture-opfl-add p1 "baz/faz/five.txt")
	(ph-venture-opfl-add p1 "six.txt")
	(ph-venture-opfl-add p1 "bar/seven.txt")

	(should-not (ph-venture-clean nil "foo"))
	(should-not (ph-venture-clean nil nil))

	(setq waste (ph-venture-clean p1 "yada"))
	(should (= 0 (length waste)))
	(should (= 7 (ph-venture-opfl-size p1)))

	(setq waste (ph-venture-clean p1 "baz"))
	(should (= 3 (length waste)))
	(should (= 4 (ph-venture-opfl-size p1)))
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

(ert-deftest ph-vl-find-by-name()
  (should-not (ph-vl-find-by-name nil))
  (should-not (ph-vl-find-by-name "foo"))

  (ph-vl-add (make-ph-ven :db "/a/bar/.ph"))
  (ph-vl-add (make-ph-ven :db "/b/x/foo/.ph"))
  (ph-vl-add (make-ph-ven :db "/c/bar/.ph"))

  (should (equal "/a/bar/.ph" (ph-ven-db (ph-vl-find-by-name "bar"))))
  (should (equal "/c/bar/.ph" (ph-ven-db (ph-vl-find-by-name "bar<2>"))))
  (should-not (ph-vl-find-by-name "DOESN'T EXIST"))

  (ph-vl-reset)
  )

(ert-deftest ph-vl-names()
  (should-not (ph-vl-names))

  (ph-vl-add (make-ph-ven :db "/a/bar/.ph"))
  (ph-vl-add (make-ph-ven :db "/b/x/foo/.ph"))
  (ph-vl-add (make-ph-ven :db "/c/bar/.ph"))

  (should (equal '("bar" "foo" "bar<2>") (ph-vl-names)))
  (ph-vl-reset)
  )

(ert-deftest ph-vl-rm()
  (let ((p1 (make-ph-ven :db "/foo/bar/.ph"))
		(p2 (make-ph-ven :db "/foobar/.ph"))
		(p3 (make-ph-ven :db "/.ph")))

	(push p1 ph-vl)
	(push p2 ph-vl)
	(push p3 ph-vl)
	(should (= 3 (ph-vl-size)))

	(should-not (ph-vl-rm nil))
	(should-not (ph-vl-rm "yada"))

	(should (ph-vl-rm "/foo/bar/.ph"))
	(should (= 2 (ph-vl-size)))

	(should (ph-vl-rm "/.ph"))
	(should (= 1 (ph-vl-size)))

	(should (ph-vl-rm "/foobar/.ph"))
	(should (= 0 (ph-vl-size)))

	(ph-vl-reset)
	))



(ert-deftest ph-db-find()
  (should-not (ph-db-find nil))
  (should-not (ph-db-find ""))
  (should-not (ph-db-find "/DOESN'T EXIST"))

  (should (equal "a/b/.ph" (ph-db-find "a/b/c/one.txt")))
  (should (equal "a/.ph" (ph-db-find "a/THIS DOESN'T EXIST/TOO")))
  (should (equal "a/.ph" (ph-db-find "a")))

  (should-not (ph-db-find "b/b/c/one.txt" ""))

  (should (equal "a/b/.ph" (ph-db-find "a/b")))
  (should (equal "b/.ph" (ph-db-find "b/b/c/one.txt")))

  (should-not (ph-db-find "empty.txt"))
  (should-not (ph-db-find "/root"))
  )

(ert-deftest ph-db-find-subproject()
  (should-not (ph-db-find-subproject nil))
  (should-not (ph-db-find-subproject ""))
  (should-not (ph-db-find-subproject "/DOESN'T EXIST"))

  (should (equal "b/.ph" (ph-db-find-subproject "b/b/c/")))
  (should (equal "b/.ph" (ph-db-find-subproject "b/b")))
  (should-not (ph-db-find-subproject "b/"))
  (should-not (ph-db-find-subproject "b"))

  (cd "b")
  (should-not (ph-db-find-subproject "./"))
  (should (equal "./.ph" (ph-db-find-subproject "b/c")))
  (should (equal "./.ph" (ph-db-find-subproject "b/")))

  (cd tdd-work-dir)
  )



(ert-run-tests-batch-and-exit (car argv))
