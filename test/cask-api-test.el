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
     (should (eq (cask-dependency-name dependency) 'bar))
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
     (should (eq (cask-dependency-name dependency-1) 'baz))
     (should (string= (cask-dependency-version dependency-1) "1.2.3"))
     (should (eq (cask-dependency-name dependency-2) 'bar))
     (should (string= (cask-dependency-version dependency-2) "0.4.3")))))

(ert-deftest cask-runtime-dependencies-test/no-cask ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/no-cask-path)))
     (should-error (cask-runtime-dependencies bundle) :type 'cask-no-cask-file))))


;;;; cask-dependencies

(ert-deftest cask-dependencies-test/package ()
  (with-sandbox
   (let* ((bundle (cask-setup cask-test/package-path))
          (dependencies (cask-dependencies bundle))
          (dependency-1 (nth 0 dependencies))
          (dependency-2 (nth 1 dependencies)))
     (should (= (length dependencies) 2))
     (should (cask-dependency-p dependency-1))
     (should (cask-dependency-p dependency-2))
     (should (eq (cask-dependency-name dependency-1) 'bar))
     (should (string= (cask-dependency-version dependency-1) "0.4.3"))
     (should (eq (cask-dependency-name dependency-2) 'baz))
     (should (string= (cask-dependency-version dependency-2) "1.2.3")))))

(ert-deftest cask-dependencies-test/config ()
  (with-sandbox
   (let* ((bundle (cask-setup cask-test/config-path))
          (dependencies (cask-dependencies bundle))
          (dependency-1 (nth 0 dependencies))
          (dependency-2 (nth 1 dependencies)))
     (should (= (length dependencies) 2))
     (should (cask-dependency-p dependency-1))
     (should (cask-dependency-p dependency-2))
     (should (eq (cask-dependency-name dependency-1) 'baz))
     (should (string= (cask-dependency-version dependency-1) "1.2.3"))
     (should (eq (cask-dependency-name dependency-2) 'bar))
     (should (string= (cask-dependency-version dependency-2) "0.4.3")))))

(ert-deftest cask-dependencies-test/no-cask ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/no-cask-path)))
     (should-error (cask-dependencies bundle) :type 'cask-no-cask-file))))


;;;; cask-development-dependencies

(ert-deftest cask-development-dependencies-test/package ()
  (with-sandbox
   (let* ((bundle (cask-setup cask-test/package-path))
          (dependencies (cask-development-dependencies bundle))
          (dependency (car dependencies)))
     (should (= (length dependencies) 1))
     (should (cask-dependency-p dependency))
     (should (eq (cask-dependency-name dependency) 'baz))
     (should (string= (cask-dependency-version dependency) "1.2.3")))))

(ert-deftest cask-development-dependencies-test/config ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/config-path)))
     (should (= (length (cask-development-dependencies bundle)) 0)))))

(ert-deftest cask-development-dependencies-test/no-cask ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/no-cask-path)))
     (should-error (cask-development-dependencies bundle) :type 'cask-no-cask-file))))


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


;;;; cask-package-name

(ert-deftest cask-package-name-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (eq (cask-package-name bundle) 'foo))))

(ert-deftest cask-package-name-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-error (cask-package-name bundle) :type 'cask-not-a-package)))


;;;; cask-package-version

(ert-deftest cask-package-version-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (string= (cask-package-version bundle) "0.8.3"))))

(ert-deftest cask-package-version-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-error (cask-package-version bundle) :type 'cask-not-a-package)))


;;;; cask-package-description

(ert-deftest cask-package-description-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (string= (cask-package-description bundle) "Foo"))))

(ert-deftest cask-package-description-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-error (cask-package-description bundle) :type 'cask-not-a-package)))


;;;; cask-version

(ert-deftest cask-version-test ()
  (should (s-matches? "^[0-9]+\.[0-9]+\.[0-9]+$" (cask-version))))


;;;; cask-elpa-dir

(ert-deftest cask-elpa-dir-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (string= (cask-elpa-dir bundle)
                     (f-join cask-test/package-path ".cask" emacs-version "elpa")))))

(ert-deftest cask-elpa-dir-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should (string= (cask-elpa-dir bundle)
                     (f-join cask-test/config-path ".cask" emacs-version "elpa")))))


;;;; cask-exec-path

(ert-deftest cask-exec-path-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should-be-colon-path (cask-exec-path bundle))))

(ert-deftest cask-exec-path-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-be-colon-path (cask-exec-path bundle))))


;;;; cask-load-path

(ert-deftest cask-load-path-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should-be-colon-path (cask-load-path bundle))))

(ert-deftest cask-load-path-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should-be-colon-path (cask-load-path bundle))))


;;;; cask-path

(ert-deftest cask-path-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (f-same? cask-test/package-path (cask-path bundle)))))

(ert-deftest cask-path-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should (f-same? cask-test/config-path (cask-path bundle)))))


;;;; cask-file

(ert-deftest cask-file-test/package ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should (f-same? (f-expand "Cask" cask-test/package-path) (cask-file bundle)))))

(ert-deftest cask-file-test/config ()
  (let ((bundle (cask-setup cask-test/config-path)))
    (should (f-same? (f-expand "Cask" cask-test/config-path) (cask-file bundle)))))


;;;; cask-caskify


;;;; cask-update


;;;; cask-install


;;;; cask-outdated


;;;; cask-initialize

(ert-deftest cask-initialize-test/package ()
  (with-sandbox
   (mock (epl-initialize) :times 1)
   (let ((bundle (cask-initialize cask-test/package-path)))
     (should (cask-bundle-p bundle)))))

(ert-deftest cask-initialize-test/config ()
  (with-sandbox
   (mock (epl-initialize) :times 1)
   (let ((bundle (cask-initialize cask-test/config-path)))
     (should (cask-bundle-p bundle)))))


;;;; cask-files

(ert-deftest cask-files-test/no-directive ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/package-path)))
     (should-not (cask-files bundle)))))

(ert-deftest cask-files-test/with-directive ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/directive-path)))
     (should (equal (cask-files bundle)
                    (--map
                     (f-expand it cask-test/directive-path)
                     '("directive-core.el" "directive.el" "bin")))))))
