(require 'el-mock)
(require 's)

(ert-deftest test-carton-setup ()
  "Should setup."
  (carton-setup
   (expand-file-name "super" carton-test-path))
  (should (equal "super" carton-project-name))
  (should (s-ends-with? "carton/test/super" carton-project-path))
  (should (s-ends-with? "carton/test/super/Carton" carton-file))
  (should (s-ends-with? "test/super/super-pkg.el" carton-package-file)))

(ert-deftest test-carton-package-user-dir-when-not-custom ()
  "Should set `package-user-dir' when not custom."
  (with-mock
   (stub file-exists-p => t)
   (stub locate-user-emacs-file => "~/.emacs.d/elpa")
   (let ((package-user-dir "~/.emacs.d/elpa"))
     (carton-setup
      (expand-file-name "super" carton-test-path))
     (should (s-ends-with? "carton/test/super/elpa" package-user-dir)))))

(ert-deftest test-carton-package-user-dir-when-custom ()
  "Should not set `package-user-dir' when custom."
  (with-mock
   (stub file-exists-p => t)
   (stub locate-user-emacs-file => "~/.emacs.d/elpa")
   (let ((package-user-dir "custom/path/to/elpa"))
     (carton-setup
      (expand-file-name "super" carton-test-path))
     (should (s-ends-with? "custom/path/to/elpa" package-user-dir)))))

(ert-deftest test-carton-package ()
  "Should define package."
  (let (carton-package)
    (carton-eval '((package "foo" "0.0.1" "Foo.")))
    (should (equal (carton-package-name carton-package) "foo"))
    (should (equal (carton-package-version carton-package) "0.0.1"))
    (should (equal (carton-package-description carton-package) "Foo."))))

(ert-deftest test-carton-package-file ()
  "Should define package file."
  (let ((carton-project-path carton-test-path)
        carton-package
        carton-runtime-dependencies carton-development-dependencies)
    (carton-eval '((package-file "sample-package.el")))
    (should (equal (carton-package-name carton-package) "sample-package"))
    (should (equal (carton-package-version carton-package) "1.0beta1"))
    (should (equal (carton-package-description carton-package)
                   "Sample package for tests"))
    (let ((package (nth 0 carton-runtime-dependencies)))
      (should (equal (carton-dependency-name package) 'foo))
      (should (equal (carton-dependency-version package) "1.0")))
    (let ((package (nth 1 carton-runtime-dependencies)))
      (should (equal (carton-dependency-name package) 'bar))
      (should (equal (carton-dependency-version package) "1.1beta3")))))

(ert-deftest test-depends-on-runtime ()
  "Should add as runtime dependency."
  (let (carton-runtime-dependencies carton-development-dependencies)
    (carton-eval '((depends-on "foo" "0.0.1")))
    (should-not carton-development-dependencies)
    (let ((package (car carton-runtime-dependencies)))
      (should (equal (carton-dependency-name package) 'foo))
      (should (equal (carton-dependency-version package) "0.0.1")))))

(ert-deftest test-depends-on-development ()
  "Should add as development dependency."
  (let (carton-runtime-dependencies carton-development-dependencies)
    (carton-eval '((development (depends-on "foo" "0.0.1"))))
    (should-not carton-runtime-dependencies)
    (let ((package (car carton-development-dependencies)))
      (should (equal (carton-dependency-name package) 'foo))
      (should (equal (carton-dependency-version package) "0.0.1")))))

(ert-deftest test-source ()
  "Should add source to `package-archives'."
  (let (package-archives)
    (carton-eval '((source "name" "url")))
    (should (equal package-archives '(("name" . "url"))))))

(ert-deftest test-carton-define-package-string ()
  "Should return correct `define-package' string."
  (let (carton-runtime-dependencies carton-package)
    (carton-eval
     '((package "foo" "0.0.1" "Foo.")
       (depends-on "bar" "0.0.2")
       (depends-on "baz" "0.0.3")))
    (should
     (equal
      (carton-define-package-string)
      "(define-package \"foo\" \"0.0.1\"\n  \"Foo.\"\n  '((bar \"0.0.2\") (baz \"0.0.3\")))\n"))))

(ert-deftest test-carton-define-package-string-no-dependencies ()
  "Should return correct `define-package' string when no dependencies."
  (let (carton-package)
    (carton-eval '((package "foo" "0.0.1" "Foo.")))
    (should
     (equal
      (carton-define-package-string)
      "(define-package \"foo\" \"0.0.1\"\n  \"Foo.\")\n"))))

(ert-deftest test-carton-dependency-string ()
  "Should return correct dependency string."
  (let (carton-runtime-dependencies)
    (carton-eval
     '((depends-on "foo" "0.0.1")
       (depends-on "bar" "0.0.2")))
    (should (equal (carton-dependency-string) "(foo \"0.0.1\") (bar \"0.0.2\")"))))

(ert-deftest test-carton-dependency-string-no-dependencies ()
  "Should return correct dependency string when no dependencies."
  (should (equal (carton-dependency-string) "")))

(ert-deftest test-carton-dependency-string-no-version ()
  "Should return correct dependency string."
  (let (carton-runtime-dependencies)
    (carton-eval '((depends-on "foo")))
    (should (equal (carton-dependency-string) "(foo \"\")"))))

(ert-deftest test-carton-unknown-directive ()
  "Should error on unknown directive."
  (let ((data (should-error (carton-eval '((foo "bar"))) :type 'error)))
    (should (equal (cadr data) "Unknown directive: (foo \"bar\")"))))

(ert-deftest test-carton-list-no-dependencies ()
  "Should print three times when no dependencies."
  (with-mock
   (mock (princ) :times 3)
   (carton-list)))

(ert-deftest test-carton-list-with-dependencies ()
  "Should print eleven times when two runtime dependencies and two development dependencies."
  (let ((carton-runtime-dependencies
         (list
          (make-carton-dependency :name "run-dep-1" :version "v1")
          (make-carton-dependency :name "run-dep-2" :version "v2")))
        (carton-development-dependencies
         (list
          (make-carton-dependency :name "dev-dep-1" :version "v1")
          (make-carton-dependency :name "dev-dep-2" :version "v2"))))
    (with-mock
     (mock (princ) :times 11)
     (carton-list))))
