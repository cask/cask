;;;; cask-setup

(ert-deftest cask-setup-test/package ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/package-path)))
     (should (cask-bundle-p bundle)))))

(ert-deftest cask-setup-test/config ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/config-path)))
     (should (cask-bundle-p bundle)))))


;;;; cask-runtime-dependencies

(ert-deftest cask-runtime-dependencies-test/package ()
  (with-sandbox
   (let* ((bundle (cask-setup cask-test/package-path))
          (dependencies (cask-runtime-dependencies bundle))
          (dependency (car dependencies)))
     (should (= (length dependencies) 1))
     (should (cask-dependency-p dependency))
     (should (string= (cask-dependency-name dependency) "bar"))
     (should (string= (cask-dependency-version dependency) "0.4.3")))))

(ert-deftest cask-runtime-dependencies-test/config ()
  (with-sandbox
   (let* ((bundle (cask-setup cask-test/config-path))
          (dependencies (cask-runtime-dependencies bundle))
          (dependency-1 (nth 0 dependencies))
          (dependency-2 (nth 1 dependencies)))
     (should (= (length dependencies) 2))
     (should (cask-dependency-p dependency-1))
     (should (cask-dependency-p dependency-2))
     (should (string= (cask-dependency-name dependency-1) "bar"))
     (should (string= (cask-dependency-version dependency-1) "0.4.3"))
     (should (string= (cask-dependency-name dependency-2) "baz"))
     (should (string= (cask-dependency-version dependency-2) "1.2.3")))))


;;;; cask-define-package-string

(ert-deftest cask-define-package-string-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (string= (cask-define-package-string bundle)
                     "(define-package \"foo\" \"0.8.3\" \"Foo\"\n  '((bar \"0.4.3\")))\n"))))

(ert-deftest cask-define-package-file-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-error (cask-define-package-string bundle) :type 'cask-not-a-package)))



;;;; cask-define-package-file

(ert-deftest cask-define-package-file-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (string= (cask-define-package-file bundle)
                     (f-expand "foo-pkg.el" cask-test/package-path)))))

(ert-deftest cask-define-package-file-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-error (cask-define-package-file bundle) :type 'cask-not-a-package)))
