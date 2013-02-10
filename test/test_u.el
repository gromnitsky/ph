:; exec emacs -Q --script "$0" -- "$@" # -*-emacs-lisp-*-

(load (expand-file-name "../ph-u.el") nil t)
(load (expand-file-name "./helper.el") nil t)



(ert-deftest ph-chomp()
  (should (equal "" (ph-chomp nil)))
  (should (equal "" (ph-chomp " ")))
  (should (equal "" (ph-chomp "		")))
  (should (equal "foo" (ph-chomp "		\nfoo	\n\n")))
  )

(ert-deftest ph-dirname()
  (should (equal "." (ph-dirname nil)))
  (should (equal "." (ph-dirname "")))
  (should (equal "." (ph-dirname "		")))

  (should (equal "/" (ph-dirname "/")))
  (should (equal "/" (ph-dirname " /foo")))
  (should (equal "/" (ph-dirname " /foo/")))
  (should (equal "/foo" (ph-dirname "/foo/bar")))
  (should (equal "/foo" (ph-dirname "/foo/bar/")))

  (should (equal "foo" (ph-dirname "foo/bar")))
  (should (equal "." (ph-dirname "foo")))
  )

(ert-deftest ph-file-relative()
  (should-error (ph-file-relative nil nil))
  (should (equal "foo" (ph-file-relative "foo" nil)))
  (should (equal "foo" (ph-file-relative " foo " " ")))

  (should (equal "foo" (ph-file-relative "/foo" "/")))
  (should (equal "bar" (ph-file-relative "/foo/bar" "/foo")))
  (should (equal "bar" (ph-file-relative "/foo/bar" "/foo/")))
  (should (equal "bar/baz" (ph-file-relative "/foo/bar/baz" "/foo")))

  (should (equal "foo" (ph-file-relative "foo" "")))
  (should (equal "bar" (ph-file-relative "foo/bar" "foo")))
  (should (equal "bar" (ph-file-relative "foo/bar" "foo/")))
  (should (equal "bar/baz" (ph-file-relative "foo/bar/baz" "foo")))

  (should-error (ph-file-relative "foo" "/"))
  (should-error (ph-file-relative "foo/bar/baz" "fox"))
  (should-error (ph-file-relative "/foo/bar/baz" "/fox"))
  )



(ert-deftest ph-vcs-init()
  (mkdir "vcs")
  (should (ph-vcs-init "vcs"))
  (should (equal (directory-files "vcs" nil "^\\.git$") (list ".git")))

  (tdd-setup-global)
)

(ert-deftest ph-vcs-detect()
  (should-not (ph-vcs-detect nil))
  (should-not (ph-vcs-detect ""))
  (should-not (ph-vcs-detect "/"))
  (should (equal 'git (ph-vcs-detect "a/")))
  )

(ert-run-tests-batch-and-exit (car argv))
