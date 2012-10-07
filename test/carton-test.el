(ert-deftest test-carton-package ()
  "Should define package."
  (let (carton-package)
    (package "foo" "0.0.1" "Foo.")
    (should (equal (carton-package-name carton-package) "foo"))
    (should (equal (carton-package-version carton-package) "0.0.1"))
    (should (equal (carton-package-description carton-package) "Foo."))))

(ert-deftest test-depends-on-runtime ()
  "Should add as runtime dependency."
  (let (carton-runtime-dependencies carton-development-dependencies)
    (depends-on "foo" "0.0.1")
    (should-not carton-development-dependencies)
    (let ((package (car carton-runtime-dependencies)))
      (should (equal (carton-dependency-name package) "foo"))
      (should (equal (carton-dependency-version package) "0.0.1")))))

(ert-deftest test-depends-on-development ()
  "Should add as development dependency."
  (let (carton-runtime-dependencies carton-development-dependencies)
    (development (depends-on "foo" "0.0.1"))
    (should-not carton-runtime-dependencies)
    (let ((package (car carton-development-dependencies)))
      (should (equal (carton-dependency-name package) "foo"))
      (should (equal (carton-dependency-version package) "0.0.1")))))

(ert-deftest test-source ()
  "Should add source to list of sources."
  (let (carton-sources)
    (source "name" "url")
    (let ((source (car carton-sources)))
      (should (equal (carton-source-name source) "name"))
      (should (equal (carton-source-url source) "url")))))

(ert-deftest test-carton-define-package-string ()
  "Should return correct `define-package' string."
  (let (carton-runtime-dependencies)
    (package "foo" "0.0.1" "Foo.")
    (depends-on "bar" "0.0.2")
    (depends-on "baz" "0.0.3")
    (should
     (equal
      (carton-define-package-string)
      "(define-package \"foo\" \"0.0.1\"\n  \"Foo.\"\n  '((\"bar\" \"0.0.2\") (\"baz\" \"0.0.3\")))\n"))))

(ert-deftest test-carton-dependency-string ()
  "Should return correct dependency string."
  (let (carton-runtime-dependencies)
    (depends-on "foo" "0.0.1")
    (depends-on "bar" "0.0.2")
    (should (equal (carton-dependency-string) "(\"foo\" \"0.0.1\") (\"bar\" \"0.0.2\")"))))
