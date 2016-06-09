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
  (defvar cask-test/link-path)
  (defvar cask-test/sandbox-path)
  (defvar cask-test/fixtures-path)
  (defvar cask-test/cvs-repo-path))


;;;; cask-setup

(ert-deftest cask-setup-test/cask-file ()
  (cask-test/with-sandbox
   (f-copy (f-join cask-test/fixtures-path "package-b-0.0.1")
           cask-test/sandbox-path)
   (let ((bundle (cask-setup (f-expand "package-b-0.0.1" cask-test/sandbox-path))))
     (should (string= (cask-package-name bundle) "package-b"))
     (should (string= (cask-package-version bundle) "0.0.1"))
     (should (string= (cask-package-description bundle) "Package-B")))))

(ert-deftest cask-setup-test/define-package-file ()
  (cask-test/with-sandbox
   (f-copy (f-join cask-test/fixtures-path "package-c-0.0.1")
           cask-test/sandbox-path)
   (let ((bundle (cask-setup (f-expand "package-c-0.0.1" cask-test/sandbox-path))))
     (should (string= (cask-package-name bundle) "package-c"))
     (should (string= (cask-package-version bundle) "0.0.1"))
     (should (string= (cask-package-description bundle) "Package-C")))))


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
      '((depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-b" "0.0.1")))
    (let ((actual (cask-runtime-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-runtime-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((depends-on "package-a" "0.0.1")
        (depends-on "package-b" "0.0.1")
        (development
         (depends-on "package-c" "0.0.1")))
    (let ((actual (cask-runtime-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1")
                          (make-cask-dependency :name 'package-b :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-runtime-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((actual (cask-runtime-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-runtime-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-test/install bundle)
    (let ((actual (cask-runtime-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))


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
      '((depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-b" "0.0.1")))
    (let ((actual (cask-development-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-b :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-development-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-b" "0.0.1")
         (depends-on "package-c" "0.0.1")))
    (let ((actual (cask-development-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-b :version "0.0.1")
                          (make-cask-dependency :name 'package-c :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-development-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (development
         (depends-on "package-c" "0.0.1")))
    (cask-install bundle)
    (let ((actual (cask-development-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-development-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (development
         (depends-on "package-c" "0.0.1")))
    (cask-test/install bundle)
    (let ((actual (cask-development-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))


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
        (depends-on "package-a" "0.0.1"))
    (let ((actual (cask-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-dependencies-test/with-multiple-dependenies ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-b" "0.0.1")))
    (let ((actual (cask-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1")
                          (make-cask-dependency :name 'package-b :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-dependencies-test/deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((actual (cask-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-c" "0.0.1")))
    (cask-test/install bundle)
    (let ((actual (cask-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1")
                          (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))


;;;; cask-installed-dependencies

(ert-deftest cask-installed-dependencies-test/not-installed ()
  (cask-test/with-bundle 'empty
    (should-not (cask-installed-dependencies bundle))))

(ert-deftest cask-installed-dependencies-test/installed ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((actual (cask-installed-dependencies bundle))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-installed-dependencies-test/installed-deep ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-installed-dependencies-test/not-in-cask-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (cask-install bundle)
    (f-mkdir (f-expand "package-b-0.0.1" (cask-elpa-path bundle)))
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-installed-dependencies-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-c "package-c-0.0.1")
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-c :version "0.0.1")
                          (make-cask-dependency :name 'package-d :version "0.0.1")
                          (make-cask-dependency :name 'package-f :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))

(ert-deftest cask-installed-dependencies-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (cask-test/install bundle)
    (let ((actual (cask-installed-dependencies bundle 'deep))
          (expected (list (make-cask-dependency :name 'package-a :version "0.0.1"))))
      (should-be-same-dependencies actual expected))))


;;;; cask-has-dependency

(ert-deftest cask-has-dependency-test/has-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (should (cask-has-dependency bundle 'package-a))))

(ert-deftest cask-has-dependency-test/does-not-have-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (should-not (cask-has-dependency bundle 'package-b))))


;;;; cask-find-dependency

(ert-deftest cask-find-dependency-test/find-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (let ((dependency (cask-find-dependency bundle 'package-a)))
      (should (eq (cask-dependency-name dependency) 'package-a))
      (should (string= (cask-dependency-version dependency) "0.0.1")))))

(ert-deftest cask-find-dependency-test/does-not-have-dependency ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (should-not (cask-find-dependency bundle 'package-b))))


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
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should (equal (read (cask-define-package-string bundle))
                   '(define-package "package-a" "0.0.1" "PACKAGE-A" 'nil)))))

(ert-deftest cask-define-package-string-test/with-dependencies ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A")
        (depends-on "package-b" "0.0.1")
        (development
         (depends-on "package-c" "0.0.1")))
    (should (equal (read (cask-define-package-string bundle))
                   '(define-package "package-a" "0.0.1" "PACKAGE-A" (quote ((package-b "0.0.1"))))))))


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
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (let ((actual-file (cask-define-package-file bundle))
          (expected-file (f-expand "package-a-pkg.el" cask-test/sandbox-path)))
      (should (string= actual-file expected-file)))))


;; cask-package-name

(ert-deftest cask-package-name-test/is-package ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should (eq (cask-package-name bundle) 'package-a))))


;;;; cask-package-version

(ert-deftest cask-package-version-test/is-package ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should (string= (cask-package-version bundle) "0.0.1"))))


;;;; cask-package-description

(ert-deftest cask-package-description-test/is-package ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should (string= (cask-package-description bundle) "PACKAGE-A"))))


;;;; cask-version

(ert-deftest cask-version-test ()
  (should (s-matches? "^[0-9]+\.[0-9]+\.[0-9]+$" (cask-version))))


;;;; cask-elpa-path

(ert-deftest cask-elpa-path-test ()
  (cask-test/with-bundle nil
    (let ((actual (cask-elpa-path bundle))
          (expected (f-join cask-test/sandbox-path
                            (format ".cask/%s.%s/elpa"
                                    emacs-major-version
                                    emacs-minor-version))))
      (should (string= actual expected)))))


;;;; cask-exec-path

(ert-deftest cask-exec-path-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-e" "0.0.1"))
    (cask-install bundle)
    (let ((path (f-join (cask-elpa-path bundle) "package-e-0.0.1" "bin")))
      (should (equal (cons path exec-path) (cask-exec-path bundle))))))

(ert-deftest cask-exec-path-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-e" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-e "package-e-0.0.1")
    (let ((path (f-join (cask-elpa-path bundle) "package-e-0.0.1" "bin")))
      (should (equal (cons path exec-path) (cask-exec-path bundle))))))

(ert-deftest cask-exec-path-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-e" "0.0.1"))
    (cask-test/install bundle)
    (cask-test/link bundle 'package-e "package-e-0.0.1")
    (let ((path (f-join (cask-elpa-path bundle) "package-e-0.0.1" "bin")))
      (should (equal (cons path exec-path) (cask-exec-path bundle))))))

(ert-deftest cask-exec-path-test/development ()
  (cask-test/with-bundle
      '((source localhost)
        (development
         (depends-on "package-e" "0.0.1")))
    (cask-install bundle)
    (cask-test/link bundle 'package-e "package-e-0.0.1")
    (let ((path (f-join (cask-elpa-path bundle) "package-e-0.0.1" "bin")))
      (should (equal (cons path exec-path) (cask-exec-path bundle))))))


;;;; cask-load-path

(ert-deftest cask-load-path-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (depends-on "package-b" "0.0.1"))
    (cask-install bundle)
    (let ((path-package-a (f-expand "package-a-0.0.1" (cask-elpa-path bundle)))
          (path-package-b (f-expand "package-b-0.0.1" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-package-b path-package-a) load-path)
        (cask-load-path bundle))))))

(ert-deftest cask-load-path-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-c "package-c-0.0.1")
    (let ((path-package-c (f-expand "package-c-0.0.1" (cask-elpa-path bundle)))
          (path-package-d (f-expand "package-d-0.0.1" (cask-elpa-path bundle)))
          (path-package-f (f-expand "package-f-0.0.1" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-package-c
                      path-package-d
                      path-package-f)
                load-path)
        (cask-load-path bundle))))))

(ert-deftest cask-load-path-test/without-initialized-environment ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (development
         (depends-on "package-b" "0.0.1")))
    (cask-test/install bundle)
    (let ((path-package-a (f-expand "package-a-0.0.1" (cask-elpa-path bundle)))
          (path-package-b (f-expand "package-b-0.0.1" (cask-elpa-path bundle))))
      (should
       (-same-items?
        (append (list path-package-b path-package-a) load-path)
        (cask-load-path bundle))))))


;;;; cask-path

(ert-deftest cask-path-test ()
  (cask-test/with-bundle nil
    (should (f-same? (cask-path bundle) cask-test/sandbox-path))))


;;;; cask-file

(ert-deftest cask-file-test ()
  (cask-test/with-bundle 'empty
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
        (depends-on "package-a" "0.0.1"))
    :packages '(("package-a" "0.0.1"))
    (cask-install bundle)
    (should-not (cask-update bundle))))

(ert-deftest cask-update-test/with-updates ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    :packages '(("package-a" "0.0.2"))
    (cask-install bundle)
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (let* ((upgrade (car (cask-update bundle)))
           (installed (epl-upgrade-installed upgrade))
           (available (epl-upgrade-available upgrade)))
      (should (eq (epl-package-name installed) 'package-a))
      (should (eq (epl-package-name available) 'package-a))
      (should (string= (epl-package-version-string installed) "0.0.1"))
      (should (string= (epl-package-version-string available) "0.0.2")))))

(ert-deftest cask-update-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-d" "0.0.1"))
    :packages '(("package-d" "0.0.1")
                ("package-f" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-d "package-d-0.0.1")
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (should-not (cask-update bundle))))

(ert-deftest cask-update-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (cask-update bundle)
      (should (equal orig-load-path load-path)))))

(ert-deftest cask-update-test/fetcher-git ()
  ;; TODO: How to test this?
  )


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
        (depends-on "package-a" "0.0.1"))
    :packages '(("package-a" "0.0.1"))
    (cask-install bundle)))

(ert-deftest cask-install-test/multiple-dependencies ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (depends-on "package-b" "0.0.1"))
    :packages '(("package-a" "0.0.1")
                ("package-b" "0.0.1"))
    (cask-install bundle)))

(ert-deftest-async cask-install-test/missing-dependencies (done)
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "missing-a" "0.0.1")
        (depends-on "missing-b" "0.0.1"))
    :packages nil
    (condition-case err
        (cask-install bundle)
      (cask-missing-dependencies
       (let ((missing-dependencies (cdr err))
             (missing-a (make-cask-dependency :name 'missing-a :version "0.0.1"))
             (missing-b (make-cask-dependency :name 'missing-b :version "0.0.1")))
         (should-be-same-dependencies missing-dependencies (list missing-a missing-b)))
       (funcall done)))))

(ert-deftest cask-install-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    :packages '(("package-c" "0.0.1")
                ("package-d" "0.0.1")
                ("package-f" "0.0.1"))
    (cask-test/link bundle 'package-c "package-c-0.0.1")
    (cask-install bundle)))

(ert-deftest cask-install-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (should (equal orig-load-path load-path)))))

(ert-deftest cask-install-test/pin-archive ()
  (if  (not (boundp 'package-pinned-packages))
    (princ "`package-pinned-packages' is not defined, we need Emacs v24.4+ to test the pinning feature.\n")
    (cask-test/with-bundle
     '((source localhost)
       (depends-on "package-a" "0.0.1" :archive "localhost"))
     (let ((archive (cask-dependency-archive
                     (car (cask-bundle-runtime-dependencies bundle)))))
       (cask-install bundle)
       (should (equal archive "localhost"))
       (should (equal package-pinned-packages
                      '((package-a . "localhost"))))))))

(ert-deftest cask-install-test/fetcher-git ()
  (cask-test/with-git-repo
   (cask-test/with-bundle
       `((depends-on "package-a" :git ,cask-test/cvs-repo-path))
     :packages '(("package-a"))
     (f-copy (f-join cask-test/fixtures-path "package-a-0.0.1" "package-a.el")
             cask-test/cvs-repo-path)
     (git "add" "package-a.el")
     (git "commit" "-a" "-m" "Add package-a.")
     (cask-install bundle))))

(ert-deftest cask-install-test/fetcher-bzr ()
  ;; TODO
  )

(ert-deftest cask-install-test/fetcher-hg ()
  ;; TODO
  )

(ert-deftest cask-install-test/fetcher-darcs ()
  ;; TODO
  )

(ert-deftest cask-install-test/fetcher-svn ()
  ;; TODO
  )

(ert-deftest cask-install-test/fetcher-cvs ()
  ;; TODO
  )

(ert-deftest cask-install-test/fetcher-files ()
  (cask-test/with-git-repo
   (cask-test/with-bundle
       `((depends-on "package-a" :git ,cask-test/cvs-repo-path :files ("package-a.el")))
     :packages '(("package-a"))
     (f-copy (f-join cask-test/fixtures-path "package-a-0.0.1" "package-a.el")
             cask-test/cvs-repo-path)
     (f-copy (f-join cask-test/fixtures-path "package-b-0.0.1" "package-b.el")
             cask-test/cvs-repo-path)
     (git "add" "package-a.el" "package-b.el")
     (git "commit" "-a" "-m" "Add package-a and package-b.")
     (cask-install bundle)
     (should (f-file? (f-expand "package-a.el" (cask-dependency-path bundle 'package-a))))
     (should-not (f-file? (f-expand "package-b.el" (cask-dependency-path bundle 'package-a)))))))

(ert-deftest cask-install-test/fetcher-branch ()
  (cask-test/with-git-repo
   (cask-test/with-bundle
       `((depends-on "package-a" :git ,cask-test/cvs-repo-path :branch "package-b"))
     :packages '(("package-a"))
     (f-copy (f-join cask-test/fixtures-path "package-a-0.0.1" "package-a.el")
             cask-test/cvs-repo-path)
     (git "add" "package-a.el")
     (git "commit" "-a" "-m" "Add package-a.")
     (git "branch" "package-b")
     (git "checkout" "package-b")
     (f-copy (f-join cask-test/fixtures-path "package-b-0.0.1" "package-b.el")
             cask-test/cvs-repo-path)
     (git "add" "package-b.el")
     (git "commit" "-a" "-m" "Add package-b.")
     (git "checkout" "master")
     (cask-install bundle)
     (should (f-file? (f-expand "package-a.el" (cask-dependency-path bundle 'package-a))))
     (should (f-file? (f-expand "package-b.el" (cask-dependency-path bundle 'package-a)))))))

(ert-deftest cask-install-test/fetcher-ref ()
  (cask-test/with-git-repo
   (f-copy (f-join cask-test/fixtures-path "package-a-0.0.1" "package-a.el")
           cask-test/cvs-repo-path)
   (git "add" "package-a.el")
   (git "commit" "-a" "-m" "Add package-a.")
   (let ((ref (s-trim (git "rev-parse" "HEAD"))))
     (cask-test/with-bundle
         `((depends-on "package-a" :git ,cask-test/cvs-repo-path :ref ,ref))
       :packages '(("package-a"))
       (git "branch" "package-b")
       (git "checkout" "package-b")
       (f-copy (f-join cask-test/fixtures-path "package-b-0.0.1" "package-b.el")
               cask-test/cvs-repo-path)
       (git "add" "package-b.el")
       (git "commit" "-a" "-m" "Add package-b.")
       (cask-install bundle)
       (should (f-file? (f-expand "package-a.el" (cask-dependency-path bundle 'package-a))))
       (should-not (f-file? (f-expand "package-b.el" (cask-dependency-path bundle 'package-a))))))))

(ert-deftest cask-install-test/fetcher-install-twice ()
  (cask-test/with-git-repo
   (cask-test/with-bundle
       `((depends-on "package-a" :git ,cask-test/cvs-repo-path))
     :packages '(("package-a"))
     (f-copy (f-join cask-test/fixtures-path "package-a-0.0.1" "package-a.el")
             cask-test/cvs-repo-path)
     (git "add" "package-a.el")
     (git "commit" "-a" "-m" "Add package-a.")
     (cask-install bundle)
     (cask-install bundle))))

(when (>= emacs-major-version 24)
  (ert-deftest cask-install-test/built-in ()
    (cask-test/with-bundle
        '((depends-on "emacs"))
      (cask-install bundle))))


;;;; cask-outdated

(ert-deftest cask-outdated-test/no-cask-file ()
  (cask-test/with-bundle nil
    :packages nil
    (should-error
     (cask-outdated bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-outdated-test/no-outdated ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    :packages '(("package-a" "0.0.1"))
    (cask-install bundle)
    (should-not (cask-outdated bundle))))

(ert-deftest cask-outdated-test/with-outdated ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    :packages '(("package-a" "0.0.1"))
    (cask-install bundle)
    (setf (cask-bundle-sources bundle) nil)
    (cask-add-source bundle "localhost" "http://127.0.0.1:9191/new-packages/")
    (let* ((epl-upgrade (car (cask-outdated bundle)))
           (installed (epl-upgrade-installed epl-upgrade))
           (available (epl-upgrade-available epl-upgrade)))
      (should (eq (epl-package-name installed) 'package-a))
      (should (eq (epl-package-name available) 'package-a))
      (should (equal (epl-package-version installed) '(0 0 1)))
      (should (equal (epl-package-version available) '(0 0 2))))))

(ert-deftest cask-outdated-test/link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-c "package-c-0.0.1")
    (should-not (cask-outdated bundle))))

(ert-deftest cask-outdated-test/intact-load-path ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (let ((orig-load-path load-path))
      (cask-install bundle)
      (cask-outdated bundle)
      (should (equal orig-load-path load-path)))))

(ert-deftest cask-outdated-test/fetcher-git ()
  ;; TODO: How to test this?
  )


;;;; cask-initialize

(ert-deftest cask-initialize-test ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1")
        (depends-on "package-b" "0.0.1"))
    (cask-install bundle)
    (should (cask-bundle-p (cask-initialize (cask-path bundle))))
    (should (equal (-map 'car package-alist) '(package-a package-b)))))


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
    (f-touch "package-a.el")
    (f-touch "package-b.el")
    (should (-same-items? (cask-files bundle) '("package-a.el" "package-b.el")))))

(ert-deftest cask-files-test/with-files-directive ()
  (cask-test/with-bundle
      '((files "package-a.el" "package-b.el"))
    (f-touch "package-a.el")
    (f-touch "package-b.el")
    (f-touch "package-c.el")
    (should (-same-items? (cask-files bundle) '("package-a.el" "package-b.el")))))


;;;; cask-add-dependency

(ert-deftest cask-add-dependency-test/without-version ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("package-a" "0.0.1")
                ("package-b" "0.0.1"))
    (cask-add-dependency bundle 'package-a)
    (cask-add-dependency bundle 'package-b)
    (cask-install bundle)))

(ert-deftest cask-add-dependency-test/runtime ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("package-a" "0.0.1")
                ("package-b" "0.0.1"))
    (cask-add-dependency bundle 'package-a :version "0.0.1")
    (cask-add-dependency bundle 'package-b :version "0.0.1")
    (cask-install bundle)))

(ert-deftest cask-add-dependency-test/development ()
  (cask-test/with-bundle
      '((source localhost))
    :packages '(("package-a" "0.0.1")
                ("package-b" "0.0.1"))
    (cask-add-dependency bundle 'package-a :version "0.0.1" :scope 'development)
    (cask-add-dependency bundle 'package-b :version "0.0.1" :scope 'development)
    (cask-install bundle)))


;;;; cask-add-source

(ert-deftest cask-add-source-test/name-and-url ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle "melpa" "https://melpa.org/packages/")
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "https://melpa.org/packages/")))))

(ert-deftest cask-add-source-test/alias ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (should-not (cask-bundle-sources bundle))
    (cask-add-source bundle 'melpa)
    (let ((source (car (cask-bundle-sources bundle))))
      (should (string= (cask-source-name source) "melpa"))
      (should (string= (cask-source-url source) "https://melpa.org/packages/")))))


;;;; cask-remove-source

(ert-deftest cask-remove-source-test/no-sources ()
  (cask-test/with-bundle nil
    (cask-remove-source bundle "package-a")
    (should-not (cask-bundle-sources bundle))))

(ert-deftest cask-remove-source-test/with-sources ()
  (cask-test/with-bundle nil
    (cask-add-source bundle "foo" "http://elpa.foo.com/packages/")
    (cask-remove-source bundle "foo")
    (should-not (cask-bundle-sources bundle))))


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

(ert-deftest cask-build-test/with-dependencies ()
  (cask-test/with-bundle '((source localhost)
                           (files "foo.el")
                           (depends-on "package-a" "0.0.1"))
    (f-write-text "(require 'package-a)" 'utf-8 "foo.el")
    (cask-install bundle)
    (cask-build bundle)))


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
        (depends-on "package-c" "0.0.1")
        (depends-on "package-d" "0.0.1"))
    (cask-install bundle)
    (let ((package-c-path (cask-test/link bundle 'package-c "package-c-0.0.1"))
          (package-d-path (cask-test/link bundle 'package-d "package-d-0.0.1")))
      (let ((actual (cask-links bundle))
            (expected `(("package-c-0.0.1" ,package-c-path)
                        ("package-d-0.0.1" ,package-d-path))))
        (should (-same-items? actual expected))))))


;;;; cask-link

(ert-deftest cask-link-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-link bundle 'package-a cask-test/sandbox-path) :type 'cask-no-cask-file)))

(ert-deftest cask-link-test/with-pkg-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((link-path (cask-test/link bundle 'package-c "package-c-0.0.1")))
      (should (f-same? (cask-dependency-path bundle 'package-c) link-path)))))

(ert-deftest cask-link-test/with-cask-file ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-b" "0.0.1"))
    (cask-install bundle)
    (let ((link-path (cask-test/link bundle 'package-b "package-b-0.0.1")))
      (should (f-same? (cask-dependency-path bundle 'package-b) link-path)))))

(ert-deftest-async cask-link-test/without-cask-and-pkg-files (done)
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (cask-install bundle)
    (condition-case err
        (cask-test/link bundle 'package-a "package-a-0.0.1")
      (error
       (should (string= (error-message-string err)
                        (format "Link source %s does not have a Cask or package-a-pkg.el file"
                                (f-expand "package-a-0.0.1" cask-test/link-path))))
       (funcall done)))))

(ert-deftest-async cask-link-test/no-such-dependency (done)
  (cask-test/with-bundle 'empty
    (condition-case err
        (cask-test/link bundle 'package-a "package-a-0.0.1")
      (error
       (should (string= (error-message-string err)
                        "Cannot link package package-a, is not a dependency"))
       (funcall done)))))

(ert-deftest-async cask-link-test/non-existing-path (done)
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (condition-case err
        (cask-link bundle 'package-a "/path/to/non-existing-directory")
      (error
       (should (string= (error-message-string err)
                        "Cannot create link package-a to non existing path: /path/to/non-existing-directory"))
       (funcall done)))))

(ert-deftest cask-link-test/already-linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-d" "0.0.1"))
    (cask-install bundle)
    (let ((link-path (cask-test/link bundle 'package-d "package-d-0.0.1")))
      (cask-link bundle 'package-d link-path)
      (should (f-same? (cask-dependency-path bundle 'package-d) link-path)))))

(ert-deftest cask-link-test/link-when-deleted ()
  (cask-test/with-bundle
   '((source localhost)
     (depends-on "package-c" "0.0.1"))
   (cask-install bundle)
   (let ((link-path (cask-test/link bundle 'package-c "package-c-0.0.1")))
     (cask-link-delete bundle 'package-c)
     (cask-test/link bundle 'package-c "package-c-0.0.1")
     (should (f-same? (cask-dependency-path bundle 'package-c) link-path)))))

(ert-deftest cask-link-test/relative-path ()
  (cask-test/with-bundle
   '((source localhost)
     (depends-on "package-d" "0.0.1"))
   (cask-install bundle)
   (let* ((link-path (cask-test/link bundle 'package-d "package-d-0.0.1"))
          (relative-link-path "./link/package-d-0.0.1"))
     (cask-link bundle 'package-d relative-link-path)
     (should (f-same? (cask-dependency-path bundle 'package-d) link-path)))))


;;;; cask-link-delete

(ert-deftest cask-link-delete-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-link-delete bundle 'package-a) :type 'cask-no-cask-file)))

(ert-deftest cask-link-delete-test/with-link ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (let ((link-path (cask-test/link bundle 'package-c "package-c-0.0.1")))
      (should (f-same? (cask-dependency-path bundle 'package-c) link-path))
      (cask-link-delete bundle 'package-c)
      (should-not (cask-dependency-path bundle 'package-c)))))

(ert-deftest-async cask-link-delete-test/no-such-dependency (done)
  (cask-test/with-bundle 'empty
    (condition-case err
        (cask-link-delete bundle 'package-a)
      (error
       (should (string= (error-message-string err)
                        "Cannot link package package-a, is not a dependency"))
       (funcall done)))))

(ert-deftest-async cask-link-delete-test/not-linked (done)
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (condition-case err
        (cask-link-delete bundle 'package-a)
      (error
       (should (string= (error-message-string err) "Package package-a not linked"))
       (funcall done)))))


;;;; cask-linked-p

(ert-deftest cask-linked-p-test/linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1"))
    (cask-install bundle)
    (cask-test/link bundle 'package-c "package-c-0.0.1")
    (should (cask-linked-p bundle 'package-c))))

(ert-deftest cask-linked-p-test/not-linked ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-a" "0.0.1"))
    (cask-install bundle)
    (should-not (cask-linked-p bundle 'package-a))))

(ert-deftest cask-linked-p-test/not-installed ()
  (cask-test/with-bundle 'empty
    (should-not (cask-linked-p bundle 'package-a))))


;;;; cask-package

(ert-deftest cask-package-test/no-cask-file ()
  (cask-test/with-bundle nil
    (should-error
     (cask-package bundle) :type 'cask-no-cask-file)))

(ert-deftest cask-package-test/not-a-package ()
  (cask-test/with-bundle 'empty
    (should-error (cask-package bundle) :type 'cask-not-a-package)))

(ert-deftest-async cask-package-test/no-files (done)
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (condition-case err
        (cask-package bundle)
      (error
       (should (s-matches? (regexp-quote "No matching file(s) found in")
                           (error-message-string err)))
       (funcall done)))))

(ert-deftest cask-package-test/without-target-dir ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (f-touch "package-a.el")
    (f-touch "package-b.el")
    (cask-package bundle)
    (let ((dist-path (f-expand "dist" (cask-path bundle))))
      (should (f-dir? dist-path))
      (should (f-file? (f-expand "package-a-0.0.1.tar" dist-path))))))

(ert-deftest cask-package-test/with-target-dir ()
  (cask-test/with-bundle
      '((package "package-a" "0.0.1" "PACKAGE-A"))
    (f-touch "package-a.el")
    (f-touch "package-b.el")
    (let ((other-path (f-expand "other" (cask-path bundle))))
      (cask-package bundle other-path)
      (should (f-dir? other-path))
      (should (f-file? (f-expand "package-a-0.0.1.tar" other-path))))))

(provide 'cask-api-test)

;;; cask-api-test.el ends here
