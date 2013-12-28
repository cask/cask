;;; cask-api-test.el --- Cask: Api tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/cask/cask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains tests for the Cask public API. Some API
;; functions are not tested here because the Ecukes tests cover them
;; well. They are still documented in this file, even if they are not
;; tested.

;;; Code:

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
   (let ((bundle (cask-setup cask-test/files-no-directive-path)))
     (should (equal (cask-files bundle)
                    (--map
                     (f-expand it cask-test/files-no-directive-path)
                     '("no-directive-core.el" "no-directive.el")))))))

(ert-deftest cask-files-test/with-directive ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/files-directive-path)))
     (should (equal (cask-files bundle)
                    (--map
                     (f-expand it cask-test/files-directive-path)
                     '("directive-core.el" "directive.el" "bin")))))))


;;;; cask-add-dependency

(ert-deftest cask-add-dependency-test/runtime ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/package-path)))
     (let* ((dependencies (cask-bundle-runtime-dependencies bundle))
            (dependency (car dependencies)))
       (should (= (length dependencies) 1))
       (should (eq (cask-dependency-name dependency) 'bar))
       (should (string= (cask-dependency-version dependency) "0.4.3")))
     (cask-add-dependency bundle 'qux "3.2.1")
     (let* ((dependencies (cask-bundle-runtime-dependencies bundle))
            (dependency-1 (nth 0 dependencies))
            (dependency-2 (nth 1 dependencies)))
       (should (= (length dependencies) 2))
       (should (eq (cask-dependency-name dependency-1) 'qux))
       (should (string= (cask-dependency-version dependency-1) "3.2.1"))
       (should (eq (cask-dependency-name dependency-2) 'bar))
       (should (string= (cask-dependency-version dependency-2) "0.4.3"))))))

(ert-deftest cask-add-dependency-test/development ()
  (with-sandbox
   (let ((bundle (cask-setup cask-test/package-path)))
     (let* ((dependencies (cask-bundle-development-dependencies bundle))
            (dependency (car dependencies)))
       (should (= (length dependencies) 1))
       (should (eq (cask-dependency-name dependency) 'baz))
       (should (string= (cask-dependency-version dependency) "1.2.3")))
     (cask-add-dependency bundle 'qux "3.2.1" :development)
     (let* ((dependencies (cask-bundle-development-dependencies bundle))
            (dependency-1 (nth 0 dependencies))
            (dependency-2 (nth 1 dependencies)))
       (should (= (length dependencies) 2))
       (should (eq (cask-dependency-name dependency-1) 'qux))
       (should (string= (cask-dependency-version dependency-1) "3.2.1"))
       (should (eq (cask-dependency-name dependency-2) 'baz))
       (should (string= (cask-dependency-version dependency-2) "1.2.3"))))))


;;;; cask-add-source

(ert-deftest cask-add-source-test/name-and-url ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle "melpa" "http://melpa.milkbox.net/packages/")
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "http://melpa.milkbox.net/packages/")))))

(ert-deftest cask-add-source-test/alias ()
  (let ((bundle (cask-setup cask-test/package-path)))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle 'melpa)
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "http://melpa.milkbox.net/packages/")))))


;;;; cask-build


;;;; cask-clean-elc


;;;; cask-links

(ert-deftest cask-links-test/no-file ()
  (with-sandbox
   (let ((cask-links-file "/path/to/non/existing/links/file"))
     (not-called f-read)
     (should-not (cask-links)))))

(ert-deftest cask-links-test/with-file-no-links ()
  (with-sandbox
   (cask-write-links nil)
   (should-not (cask-links))))

(ert-deftest cask-links-test/with-file-with-links ()
  (with-sandbox
   (let ((links '(("foo" . "/path/to/foo")
                  ("bar" . "/path/to/bar"))))
     (cask-write-links links)
     (should (equal (cask-links) links)))))


;;;; cask-link-p

(ert-deftest cask-link-p-test/no-file ()
  (with-sandbox
   (let ((cask-links-file "/path/to/non/existing/links/file"))
     (should-not (cask-link-p "foo")))))

(ert-deftest cask-link-p-test/with-file-no-links ()
  (with-sandbox
   (cask-write-links nil)
   (should-not (cask-link-p "foo"))))

(ert-deftest cask-link-p-test/with-file-does-not-exist ()
  (with-sandbox
   (cask-write-links '(("bar" . "/path/to/bar")))
   (should-not (cask-link-p "foo"))))

(ert-deftest cask-link-p-test/with-file-exists ()
  (with-sandbox
   (cask-write-links '(("foo" . "/path/to/foo")))
   (should (cask-link-p "foo"))))


;;;; cask-link

(ert-deftest cask-link-test/no-name ()
  (with-sandbox
   (let ((bundle-1 (make-cask-bundle :name 'foo :path "/path/to/foo"))
         (bundle-2 (make-cask-bundle :name 'bar :path "/path/to/bar")))
     (cask-link bundle-1)
     (cask-link bundle-2)
     (should (equal (cask-links) '(("bar" "/path/to/bar")
                                   ("foo" "/path/to/foo")))))))

(ert-deftest cask-link-test/with-name-has-not-been-linked ()
  (with-sandbox
   (let ((bundle-1 (make-cask-bundle :name 'foo :path "/path/to/foo"))
         (bundle-2 (make-cask-bundle :name 'bar :path "/path/to/bar")))
     (should-error (cask-link bundle-2 "foo") :type 'cask-no-such-link))))

(ert-deftest cask-link-test/with-name-has-been-linked ()
  (with-sandbox
   (let ((emacs-version "24.3.1"))
     (stub f-delete)
     (mock (f-symlink "/path/to/foo" "/path/to/bar/.cask/24.3.1/elpa/foo-dev"))
     (let ((bundle-1 (make-cask-bundle :name 'foo :path "/path/to/foo"))
           (bundle-2 (make-cask-bundle :name 'bar :path "/path/to/bar")))
       (cask-link bundle-1)
       (cask-link bundle-2 "foo")))))

(ert-deftest cask-link-test/with-name-delete-existing ()
  (with-sandbox
   (mock (f-delete) :times 2)
   (stub f-symlink)
   (stub f-glob => '("/path/to/foo-1" "/path/to/foo-2"))
   (let ((bundle-1 (make-cask-bundle :name 'foo :path "/path/to/foo"))
         (bundle-2 (make-cask-bundle :name 'bar :path "/path/to/bar")))
     (cask-link bundle-1)
     (cask-link bundle-2 "foo"))))


;;;; cask-link-delete

(ert-deftest cask-link-delete-test/no-file ()
  (with-sandbox
   (let ((cask-links-file "/path/to/non/existing/links/file"))
     (should-error (cask-link-delete '("foo")) :type 'cask-no-such-link)
     (should-not (f-file? cask-links-file)))))

(ert-deftest cask-link-delete-test/with-file-no-link ()
  (with-sandbox
   (cask-write-links '(("foo" . "/path/to/foo")))
   (should-error (cask-link-delete '("bar")) :type 'cask-no-such-link)))

(ert-deftest cask-link-delete-test/with-file-single-link ()
  (with-sandbox
   (cask-write-links '(("foo" . "/path/to/foo")))
   (should (cask-link-p "foo"))
   (cask-link-delete '("foo"))
   (should-not (cask-link-p "foo"))))

(ert-deftest cask-link-delete-test/with-file-multiple-links ()
  (with-sandbox
   (cask-write-links '(("foo" . "/path/to/foo")
                       ("bar" . "/path/to/bar")))
   (should (cask-link-p "foo"))
   (should (cask-link-p "bar"))
   (cask-link-delete '("foo" "bar"))
   (should-not (cask-link-p "foo"))
   (should-not (cask-link-p "bar"))))

;;; cask-api-test.el ends here
