;;; carton.el --- Emacs dependency management made easy

;; Copyright (C) 2012, 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.1.2
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/carton

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'package)
(eval-when-compile
  (require 'cl))

(defstruct carton-package name version description)
(defstruct carton-dependency name version)
(defstruct carton-source name url)

(defvar carton-project-path nil
  "Path to project.")

(defvar carton-project-name nil
  "Name of project.")

(defvar carton-file nil
  "Path to `Carton` file.")

(defvar carton-package-file nil
  "Path to project package (`-pkg.el`) file.")

(defvar carton-development nil
  "Development dependency or not.")

(defvar carton-development-dependencies nil
  "List of development dependencies.")

(defvar carton-runtime-dependencies nil
  "List of runtime dependencies.")

(defvar carton-package nil
  "Project package information.")

(defun carton-read (filename)
  "Read a carton file from FILENAME.

Return all directives in the Carton file as list."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (format "(%s)" (buffer-substring-no-properties (point-min) (point-max))))))

(defun carton-get-dep-list-for-scope (scope)
  "Get the dependency list symbol for SCOPE."
  (if (eq scope :development)
      'carton-development-dependencies
    'carton-runtime-dependencies))

(defun carton-add-dependency (name &optional version scope)
  "Add the dependency NAME with VERSION in SCOPE."
  (let* ((name (if (stringp name) (intern name) name))
         (dependency (make-carton-dependency :name name :version version))
         (dep-list (carton-get-dep-list-for-scope scope)))
    (add-to-list dep-list dependency t)))

(defun carton-parse-package-info (info)
  "Parse package INFO as returned by `package-buffer-info'."
  ;; We have to convert the INFO vector to a list first, because pcase does not
  ;; support pattern matching on vectors.
  (pcase-let* ((info (append info nil))
               (`(,name ,requires ,description ,version _) info))
    (setq carton-package (make-carton-package :name name
                                              :version version
                                              :description description))
    (dolist (req requires)
      (pcase-let ((`(,name ,version) req))
        (carton-add-dependency name (package-version-join version))))))

(defun carton-parse-package-file (filename)
  "Parse a package file from FILENAME.

Extract name, version, description and runtime dependencies from
the package headers in FILENAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename carton-project-path))
    (carton-parse-package-info (package-buffer-info))))

(defun carton-eval (forms &optional scope)
  "Evaluate carton FORMS in SCOPE.

SCOPE may be nil or :development."
  (dolist (form forms)
    (pcase form
      (`(source ,name ,url)
       (add-to-list 'package-archives (cons name url)))
      (`(package ,name ,version ,description)
       (setq carton-package (make-carton-package :name name
                                                 :version version
                                                 :description description)))
      (`(package-file ,filename) (carton-parse-package-file filename))
      (`(depends-on ,name ,version) (carton-add-dependency name version scope))
      (`(depends-on ,name) (carton-add-dependency name nil scope))
      (`(development . ,body) (carton-eval body :development))
      (_ (error "Unknown directive: %S" form)))))

(defun carton-setup (project-path)
  "Setup carton for project at PROJECT-PATH."
  (setq carton-project-path (directory-file-name project-path))
  (setq carton-project-name (file-name-nondirectory carton-project-path))
  (setq carton-file (expand-file-name "Carton" carton-project-path))
  (setq carton-package-file (expand-file-name (concat carton-project-name "-pkg.el") carton-project-path))
  (when (equal (eval (car (get 'package-user-dir 'standard-value))) package-user-dir)
    (setq package-user-dir (expand-file-name "elpa" carton-project-path)))
  (unless (file-exists-p carton-file)
    (error "Could not locate `Carton` file")
    (kill-emacs 1))
  (carton-eval (carton-read carton-file)))

(defun carton-install ()
  "Install dependencies."
  (let ((carton-dependencies (append carton-development-dependencies carton-runtime-dependencies)))
    (when carton-dependencies
      (package-refresh-contents)
      (package-initialize)
      (mapc
       (lambda (package)
         (let ((name (carton-dependency-name package)))
           (unless (package-installed-p name)
             (package-install name))))
       carton-dependencies))))

(defun carton-update ()
  "Update packages that have new versions."
  (with-temp-buffer
    (package-refresh-contents)
    (package-initialize)
    (package-menu--generate nil t) ;; WTF ELPA, really???
    (mapc
     (lambda (package)
       (package-install (car package)))
     (package-menu--find-upgrades))
    ;; Delete obsolete packages
    (mapc
     (lambda (package)
       (package-delete (symbol-name (car package))
                       (package-version-join (caadr package))))
     package-obsolete-alist)))

(defun carton-version ()
  "Print the version of this project."
  (princ (format "%s\n" (carton-package-version carton-package))))

(defun carton-package ()
  "Package this project."
  (let ((content (carton-define-package-string)))
    (with-temp-file carton-package-file (insert content))))

(defun carton-define-package-string ()
  "Return `define-package' string."
  (format
   "(define-package \"%s\" \"%s\"\n  \"%s\"%s)\n"
   (carton-package-name carton-package)
   (carton-package-version carton-package)
   (carton-package-description carton-package)
   (let ((dependency-string (carton-dependency-string)))
     (if (equal dependency-string "")
         "" (format "\n  '(%s)" dependency-string)))))

(defun carton-dependency-string ()
  "Return dependencies as string."
  (mapconcat
   (lambda (package)
     (let ((name (carton-dependency-name package))
           (version (carton-dependency-version package)))
       (format "(%s \"%s\")" name (or version ""))))
   carton-runtime-dependencies " "))

(provide 'carton)

;;; carton.el ends here
