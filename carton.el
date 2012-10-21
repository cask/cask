;;; carton.el --- Emacs dependency management made easy

;; Copyright (C) 2012 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
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

(defun carton-setup (project-path)
  "Setup carton for project at PROJECT-PATH."
  (setq carton-project-path (directory-file-name project-path))
  (setq carton-project-name (file-name-nondirectory carton-project-path))
  (setq carton-file (expand-file-name "Carton" carton-project-path))
  (setq carton-package-file (expand-file-name (concat carton-project-name "-pkg.el") carton-project-path))
  (setq package-user-dir (expand-file-name "elpa" carton-project-path))
  (unless (file-exists-p carton-file)
    (error "Could not locate `Carton` file.")
    (kill-emacs 1))
  (load carton-file t t t))

(defun source (name url)
  "Add source with NAME and URL."
  (add-to-list 'package-archives `(,name . ,url)))

(defun package (name version description)
  "Define package with NAME, VERSION and DESCRIPTION."
  (setq carton-package (make-carton-package :name name :version version :description description)))

(defmacro development (&rest body)
  "Scope to development dependencies."
  `(let ((carton-development t)) ,@body))

(defun depends-on (name &optional version)
  "Add dependency with NAME and VERSION."
  (let ((dependency (make-carton-dependency :name (intern name) :version version))
        (dependency-list
         (if carton-development 'carton-development-dependencies 'carton-runtime-dependencies)))
    (add-to-list dependency-list dependency t)))

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
  (package-refresh-contents)
  (package-initialize)
  (package-menu--generate nil nil) ;; WTF ELPA, really???
  (mapc
   (lambda (package)
     (package-install (car package)))
   (package-menu--find-upgrades)))

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

;;; carton.el ends here
