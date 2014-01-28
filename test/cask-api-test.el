;;; cask-api-test.el --- Cask: Api tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Johan Andersson

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

(require 'cask)

(eval-when-compile
  (defvar cask-test/sandbox-path))


;;;; cask-setup

(ert-deftest cask-setup-test/returns-bundle ()
  (cask-test/with-sandbox
   (let ((bundle (cask-setup cask-test/sandbox-path)))
     (should (cask-bundle-p bundle)))))


;;;; cask-runtime-dependencies

(ert-deftest cask-runtime-dependencies-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-runtime-dependencies bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-runtime-dependencies-test/no-dependencies ()
  (cask-test/with-bundle 'empty
    (should-not (cask-runtime-dependencies bundle))))

(ert-deftest cask-runtime-dependencies-test/with-single-dependency ()
  (cask-test/with-bundle
      '((depends-on "foo" "0.0.1")
        (development
         (depends-on "bar" "0.0.2")))
    (let ((actual (cask-runtime-dependencies bundle))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-runtime-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2")
        (development
         (depends-on "baz" "0.0.3")))
    (let ((actual (cask-runtime-dependencies bundle))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1")
                          (make-cask-dependency :name 'bar :version "0.0.2"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-runtime-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "baz" "0.0.3"))
    (cask-install bundle)
    (let ((actual (cask-runtime-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-runtime-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "baz" "0.0.3"))
    (cask-test/install bundle)
    (let ((actual (cask-runtime-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))


;;;; cask-development-dependencies

(ert-deftest cask-development-dependencies-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-development-dependencies bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-development-dependencies-test/no-dependencies ()
  (cask-test/with-bundle 'empty
    (should-not (cask-development-dependencies bundle))))

(ert-deftest cask-development-dependencies-test/with-single-dependency ()
  (cask-test/with-bundle
      '((depends-on "foo" "0.0.1")
        (development
         (depends-on "bar" "0.0.2")))
    (let ((actual (cask-development-dependencies bundle))
          (expected (list (make-cask-dependency :name 'bar :version "0.0.2"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-development-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((depends-on "foo" "0.0.1")
        (development
         (depends-on "bar" "0.0.2")
         (depends-on "baz" "0.0.3")))
    (let ((actual (cask-development-dependencies bundle))
          (expected (list (make-cask-dependency :name 'bar :version "0.0.2")
                          (make-cask-dependency :name 'baz :version "0.0.3"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-development-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (development
         (depends-on "baz" "0.0.3")))
    (cask-install bundle)
    (let ((actual (cask-development-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-development-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (development
         (depends-on "baz" "0.0.3")))
    (cask-test/install bundle)
    (let ((actual (cask-development-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))


;;;; cask-dependencies

(ert-deftest cask-dependencies-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-dependencies bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-dependencies-test/no-dependencies ()
  (cask-test/with-bundle 'empty
    (should-not (cask-dependencies bundle))))

(ert-deftest cask-dependencies-test/with-single-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (let ((actual (cask-dependencies bundle))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (development
         (depends-on "bar" "0.0.2")))
    (let ((actual (cask-dependencies bundle))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1")
                          (make-cask-dependency :name 'bar :version "0.0.2"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "baz" "0.0.3"))
    (cask-install bundle)
    (let ((actual (cask-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (development
         (depends-on "baz" "0.0.3")))
    (cask-test/install bundle)
    (let ((actual (cask-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1")
                          (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))


;;;; cask-installed-dependencies

(ert-deftest cask-installed-dependencies-test/not-installed ()
  (cask-test/with-bundle 'empty
    (should-not (cask-installed-dependencies bundle))))

(ert-deftest cask-installed-dependencies-test/installed ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "baz" "0.0.3"))
    (cask-install bundle)
    (let ((actual (cask-installed-dependencies bundle))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-installed-dependencies-test/installed-deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "baz" "0.0.3"))
    (cask-install bundle)
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'baz :version "0.0.3")
                          (make-cask-dependency :name 'qux :version "0.0.4")
                          (make-cask-dependency :name 'fux :version "0.0.6"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-installed-dependencies-test/no-directory ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (f-delete (cask-dependency-path bundle 'foo) 'force)
    (should-not (cask-installed-dependencies bundle))))

(ert-deftest cask-installed-dependencies-test/not-in-cask-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (f-mkdir (f-expand "bar-0.0.2" (cask-elpa-path bundle)))
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-installed-dependencies-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1"))))
      (should (-same-items? actual expected)))))

(ert-deftest cask-installed-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-test/install bundle)
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'foo :version "0.0.1"))))
      (should (-same-items? actual expected)))))


;;;; cask-define-package-string

(ert-deftest cask-define-package-string-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-define-package-string bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-define-package-string-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-define-package-file bundle) :type 'cask-not-a-package)))

(ert-deftest cask-define-package-string-test/no-dependencies ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should (equal (read (cask-define-package-string bundle))
                   '(define-package "foo" "0.0.1" "FOO" 'nil)))))

(ert-deftest cask-define-package-string-test/with-dependencies ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO")
        (depends-on "bar" "0.0.2")
        (development
         (depends-on "baz" "0.0.3")))
    (should (equal (read (cask-define-package-string bundle))
                   '(define-package "foo" "0.0.1" "FOO" (quote ((bar "0.0.2"))))))))


;;;; cask-define-package-file

(ert-deftest cask-define-package-file-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-define-package-file bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-define-package-file-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-define-package-file bundle) :type 'cask-not-a-package)))

(ert-deftest cask-define-package-file-test/with-package-directive ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (let ((actual-file (cask-define-package-file bundle))
          (expected-file (f-expand "foo-pkg.el" cask-test/sandbox-path)))
      (should (string= actual-file expected-file)))))


;; cask-package-name

(ert-deftest cask-package-name-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-package-name bundle) :type 'cask-not-a-package)))

(ert-deftest cask-package-name-test/is-package ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should (eq (cask-package-name bundle) 'foo))))


;;;; cask-package-version

(ert-deftest cask-package-version-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-package-version bundle) :type 'cask-not-a-package)))

(ert-deftest cask-package-version-test/is-package ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should (string= (cask-package-version bundle) "0.0.1"))))


;;;; cask-package-description

(ert-deftest cask-package-description-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-package-description bundle) :type 'cask-not-a-package)))

(ert-deftest cask-package-description-test/is-package ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should (string= (cask-package-description bundle) "FOO"))))


;;;; cask-version

(ert-deftest cask-version-test ()
  (should (s-matches? "^[0-9]+\.[0-9]+\.[0-9]+$" (cask-version))))


;;;; cask-elpa-path

(ert-deftest cask-elpa-path-test ()
  (cask-test/with-bundle nil
    (let ((actual (cask-elpa-path bundle))
          (expected (f-join cask-test/sandbox-path ".cask" emacs-version "elpa")))
      (should (string= actual expected)))))


;;;; cask-exec-path

(ert-deftest cask-exec-path-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "hey" "0.0.5"))
    (cask-install bundle)
    (let ((path (f-join (cask-elpa-path bundle) "hey-0.0.5" "bin")))
      (should (equal (cons path exec-path) (cask-exec-path bundle))))))

(ert-deftest cask-exec-path-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "hey" "0.0.5"))
    (cask-install bundle)
    (let ((hey-path (f-expand "hey" cask-test/sandbox-path)))
      (f-copy (f-expand "hey-0.0.5" (cask-elpa-path bundle)) hey-path)
      (cask-link bundle 'hey hey-path)
      (let ((path (f-join (cask-elpa-path bundle) "hey-0.0.5" "bin")))
        (should (equal (cons path exec-path) (cask-exec-path bundle)))))))

(ert-deftest cask-exec-path-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "hey" "0.0.5"))
    (cask-test/install bundle)
    (let ((hey-path (f-expand "hey" cask-test/sandbox-path)))
      (f-copy (f-expand "hey-0.0.5" (cask-elpa-path bundle)) hey-path)
      (cask-link bundle 'hey hey-path)
      (let ((path (f-join (cask-elpa-path bundle) "hey-0.0.5" "bin")))
        (should (equal (cons path exec-path) (cask-exec-path bundle)))))))


;;;; cask-load-path

(ert-deftest cask-load-path-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-install bundle)
    (let ((path-foo (f-expand "foo-0.0.1" (cask-elpa-path bundle)))
          (path-bar (f-expand "bar-0.0.2" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-bar path-foo) load-path)
        (cask-load-path bundle))))))

(ert-deftest cask-load-path-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (let ((path-foo (f-expand "foo-0.0.1" (cask-elpa-path bundle)))
          (path-bar (f-expand "bar-0.0.2" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-bar path-foo) load-path)
        (cask-load-path bundle))))))

(ert-deftest cask-load-path-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-test/install bundle)
    (let ((path-foo (f-expand "foo-0.0.1" (cask-elpa-path bundle)))
          (path-bar (f-expand "bar-0.0.2" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-bar path-foo) load-path)
        (cask-load-path bundle))))))


;;;; cask-path

(ert-deftest cask-path-test ()
  (cask-test/with-bundle nil
    (should (f-same? (cask-path bundle) cask-test/sandbox-path))))


;;;; cask-file

(ert-deftest cask-file-test ()
  (cask-test/with-bundle nil
    (should (f-same? (cask-file bundle) (f-expand "Cask" cask-test/sandbox-path)))))


;;;; cask-caskify

(ert-deftest cask-caskify-test ()
  (cask-test/with-bundle nil
    (should-not (f-file? (f-expand "Cask" cask-test/sandbox-path)))
    (cask-caskify bundle)
    (should (f-file? (f-expand "Cask" cask-test/sandbox-path)))))


;;;; cask-update

(ert-deftest cask-update-test/no-cask-file ()
  (cask-test/with-bundle nil
    :packages nil
    (should-error
     (cask-update bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-update-test/no-update ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (cask-update bundle)))

(ert-deftest cask-update-test/with-updates ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.2"))
    (cask-install bundle)
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (cask-update bundle)))

(ert-deftest cask-update-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (cask-update bundle)))

(ert-deftest cask-update-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (cask-update bundle)
      (should (equal orig-load-path load-path)))))


;;;; cask-install

(ert-deftest cask-install-test/no-cask-file ()
  (cask-test/with-bundle nil
    :packages nil
    (should-error
     (cask-install bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-install-test/no-dependencies ()
  (cask-test/with-bundle 'empty
    :packages nil
    (cask-install bundle)))

(ert-deftest cask-install-test/single-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)))

(ert-deftest cask-install-test/multiple-dependencies ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    :packages '(("foo" "0.0.1")
                ("bar" "0.0.2"))
    (cask-install bundle)))

(ert-deftest cask-install-test/missing-dependencies ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "missing-a" "0.0.1")
        (depends-on "missing-b" "0.0.2"))
    :packages nil
    (condition-case err
        (cask-install bundle)
      (cask-missing-dependencies
       (let ((missing-dependencies (cdr err))
             (missing-a (make-cask-dependency :name 'missing-a :version "0.0.1"))
             (missing-b (make-cask-dependency :name 'missing-b :version "0.0.2")))
         (should (-same-items? missing-dependencies (list missing-a missing-b))))))))

(ert-deftest cask-install-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (cask-install bundle)))

(ert-deftest cask-install-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (should (equal orig-load-path load-path)))))


;;;; cask-outdated

(ert-deftest cask-outdated-test/no-cask-file ()
  (cask-test/with-bundle nil
    :packages nil
    (should-error
     (cask-outdated bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-outdated-test/no-outdated ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (should-not (cask-outdated bundle))))

(ert-deftest cask-outdated-test/with-outdated ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (let* ((epl-upgrade (car (cask-outdated bundle)))
           (installed (epl-upgrade-installed epl-upgrade))
           (available (epl-upgrade-available epl-upgrade)))
      (should (eq (epl-package-name installed) 'foo))
      (should (eq (epl-package-name available) 'foo))
      (should (equal (epl-package-version installed) '(0 0 1)))
      (should (equal (epl-package-version available) '(0 0 2))))))

(ert-deftest cask-outdated-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    :packages '(("foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (should-not (cask-outdated bundle))))

(ert-deftest cask-outdated-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (cask-outdated bundle)
      (should (equal orig-load-path load-path)))))


;;;; cask-initialize

(ert-deftest cask-initialize-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (should (cask-bundle-p (cask-initialize (cask-path bundle))))
    (should (equal (-map 'car package-alist) '(foo)))))

(ert-deftest cask-initialize-test/only-installed ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-install bundle)
    (f-delete (cask-dependency-path bundle 'foo) 'force)
    (cask-initialize (cask-path bundle))
    (should (equal (-map 'car package-alist) '(bar)))))

(ert-deftest cask-initialize-test/link-with-define-package-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (let ((foo-path (f-expand "foo" cask-test/sandbox-path)))
      (f-copy (cask-dependency-path bundle 'foo) foo-path)
      (cask-link bundle 'foo foo-path))
    (cask-initialize (cask-path bundle))
    (should (equal (-map 'car package-alist) '(foo)))))

(ert-deftest cask-initialize-test/link-no-define-package-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (let ((foo-path (f-expand "foo" cask-test/sandbox-path)))
      (f-mkdir foo-path)
      (cask-link bundle 'foo foo-path))
    (cask-initialize (cask-path bundle))
    (should-not package-alist)))


;;;; cask-files

(ert-deftest cask-files-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-files bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-files-test/no-files-directive-no-files ()
  (cask-test/with-bundle 'empty
    (should-not (cask-files bundle))))

(ert-deftest cask-files-test/no-files-directive-with-files ()
  (cask-test/with-bundle 'empty
    (f-touch "foo.el")
    (f-touch "bar.el")
    (should (-same-items? (cask-files bundle) '("foo.el" "bar.el")))))

(ert-deftest cask-files-test/with-files-directive ()
  (cask-test/with-bundle
      '((files "foo.el" "bar.el"))
    (f-touch "foo.el")
    (f-touch "bar.el")
    (f-touch "baz.el")
    (should (-same-items? (cask-files bundle) '("foo.el" "bar.el")))))


;;;; cask-add-dependency

(ert-deftest cask-add-dependency-test/without-version ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("foo" "0.0.1")
                ("bar" "0.0.2"))
    (cask-add-dependency bundle 'foo)
    (cask-add-dependency bundle 'bar)
    (cask-install bundle)))

(ert-deftest cask-add-dependency-test/runtime ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("foo" "0.0.1")
                ("bar" "0.0.2"))
    (cask-add-dependency bundle 'foo "0.0.1")
    (cask-add-dependency bundle 'bar "0.0.2")
    (cask-install bundle)))

(ert-deftest cask-add-dependency-test/development ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("foo" "0.0.1")
                ("bar" "0.0.2"))
    (cask-add-dependency bundle 'foo "0.0.1" :development)
    (cask-add-dependency bundle 'bar "0.0.2" :development)
    (cask-install bundle)))


;;;; cask-add-source

(ert-deftest cask-add-source-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-add-source bundle 'foo) :type 'cask-no-cask-file)))

(ert-deftest cask-add-source-test/name-and-url ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle "melpa" "http://melpa.milkbox.net/packages/")
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "http://melpa.milkbox.net/packages/")))))

(ert-deftest cask-add-source-test/alias ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle 'melpa)
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "http://melpa.milkbox.net/packages/")))))


;;;; cask-build

(ert-deftest cask-build-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-build bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-build-test/with-files-directive ()
  (cask-test/with-bundle
      '((files "foo.el"))
    (f-touch "foo.el")
    (f-touch "bar.el")
    (cask-build bundle)
    (should (f-file? "foo.el"))
    (should (f-file? "foo.el"))
    (should (f-file? "bar.el"))
    (should-not (f-file? "bar.elc"))))

(ert-deftest cask-build-test/no-files-directive ()
  (cask-test/with-bundle 'empty
    (f-touch "foo.el")
    (f-touch "bar.el")
    (cask-build bundle)
    (should (f-file? "foo.el"))
    (should (f-file? "foo.el"))
    (should (f-file? "bar.el"))
    (should (f-file? "bar.elc"))))


;;;; cask-clean-elc

(ert-deftest cask-clean-elc-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-clean-elc bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-clean-elc-test/with-files-directive ()
  (cask-test/with-bundle
      '((files "foo.el"))
    (f-touch "foo.el")
    (f-touch "foo.elc")
    (f-touch "bar.el")
    (f-touch "bar.elc")
    (cask-clean-elc bundle)
    (should (f-file? "foo.el"))
    (should-not (f-file? "foo.elc"))
    (should (f-file? "bar.el"))
    (should (f-file? "bar.elc"))))

(ert-deftest cask-clean-elc-test/without-files-directive ()
  (cask-test/with-bundle 'empty
    (f-touch "foo.el")
    (f-touch "foo.elc")
    (f-touch "bar.el")
    (f-touch "bar.elc")
    (cask-clean-elc bundle)
    (should (f-file? "foo.el"))
    (should-not (f-file? "foo.elc"))
    (should (f-file? "bar.el"))
    (should-not (f-file? "bar.elc"))))


;;;; cask-links

(ert-deftest cask-links-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-links bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-links-test/not-initialized ()
  (cask-test/with-bundle 'empty
    (should-not (cask-links bundle))))

(ert-deftest cask-links-test/no-links ()
  (cask-test/with-bundle 'empty
    (cask-install bundle)
    (should-not (cask-links bundle))))

(ert-deftest cask-links-test/with-links ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (cask-link bundle 'bar cask-test/sandbox-path)
    (let ((actual (cask-links bundle))
          (expected `(("foo-0.0.1" ,cask-test/sandbox-path)
                      ("bar-0.0.2" ,cask-test/sandbox-path))))
      (should (-same-items? actual expected)))))


;;;; cask-link

(ert-deftest cask-link-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-link bundle 'foo cask-test/sandbox-path) :type 'cask-no-cask-file)))

(ert-deftest cask-link-test/valid-links ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (cask-link bundle 'bar cask-test/sandbox-path)
    (should (f-same? (cask-dependency-path bundle 'foo) cask-test/sandbox-path))
    (should (f-same? (cask-dependency-path bundle 'bar) cask-test/sandbox-path))))

(ert-deftest cask-link-test/no-such-dependency ()
  (cask-test/with-bundle 'empty
    (condition-case err
        (cask-link bundle 'foo cask-test/sandbox-path)
      (error
       (should (string= (error-message-string err)
                        "Cannot link package foo, is not a dependency"))))))

(ert-deftest cask-link-test/non-existing-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (condition-case err
        (cask-link bundle 'foo "/path/to/non-existing-directory")
      (error
       (should (string= (error-message-string err)
                        "Cannot create link foo to non existing path: /path/to/non-existing-directory"))))))

(ert-deftest cask-link-test/already-linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (condition-case err
        (cask-link bundle 'foo cask-test/sandbox-path)
      (error
       (should (string= (error-message-string err)
                        "Package foo has already been linked"))))))


;;;; cask-link-delete

(ert-deftest cask-link-delete-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-link-delete bundle 'foo) :type 'cask-no-cask-file)))

(ert-deftest cask-link-delete-test/valid-links ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1")
        (depends-on "bar" "0.0.2"))
    :packages '(("foo" "0.0.1")
                ("bar" "0.0.2"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (cask-link bundle 'bar cask-test/sandbox-path)
    (should (f-same? (cask-dependency-path bundle 'foo) cask-test/sandbox-path))
    (should (f-same? (cask-dependency-path bundle 'bar) cask-test/sandbox-path))
    (cask-link-delete bundle 'foo)
    (cask-link-delete bundle 'bar)
    (should-not (f-same? (cask-dependency-path bundle 'foo) cask-test/sandbox-path))
    (should-not (f-same? (cask-dependency-path bundle 'bar) cask-test/sandbox-path))))

(ert-deftest cask-link-delete-test/no-such-dependency ()
  (cask-test/with-bundle 'empty
    (condition-case err
        (cask-link-delete bundle 'foo)
      (error
       (should (string= (error-message-string err)
                        "Cannot link package foo, is not a dependency"))))))

(ert-deftest cask-link-delete-test/not-linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (condition-case err
        (cask-link-delete bundle 'foo)
      (error
       (should (string= (error-message-string err) "Package foo not linked"))))))


;;;; cask-linked-p

(ert-deftest cask-linked-p-test/linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (cask-link bundle 'foo cask-test/sandbox-path)
    (should (cask-linked-p bundle 'foo))))

(ert-deftest cask-linked-p-test/not-linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "foo" "0.0.1"))
    (cask-install bundle)
    (should-not (cask-linked-p bundle 'foo))))


;;;; cask-package

(ert-deftest cask-package-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-package bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-package-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-package bundle) :type 'cask-not-a-package)))

(ert-deftest cask-package-test/no-files ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (condition-case err
        (cask-package bundle)
      (error
       (should (s-matches? (regexp-quote "No matching file(s) found in")
                           (error-message-string err)))))))

(ert-deftest cask-package-test/without-target-dir ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (f-touch "foo.el")
    (f-touch "bar.el")
    (cask-package bundle)
    (let ((dist-path (f-expand "dist" (cask-path bundle))))
      (should (f-dir? dist-path))
      (should (f-file? (f-expand "foo-0.0.1.tar" dist-path))))))

(ert-deftest cask-package-test/with-target-dir ()
  (cask-test/with-bundle
      '((package "foo" "0.0.1" "FOO"))
    (f-touch "foo.el")
    (f-touch "bar.el")
    (let ((other-path (f-expand "other" (cask-path bundle))))
      (cask-package bundle other-path)
      (should (f-dir? other-path))
      (should (f-file? (f-expand "foo-0.0.1.tar" other-path))))))

(provide 'cask-api-test)

;;; cask-api-test.el ends here
