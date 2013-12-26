;;; cask.el --- Cask: Emacs dependency management made easy  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner
;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.5.1
;; Keywords: speed, convenience
;; URL: http://github.com/cask/cask
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.10.0") (epl "0.0.1") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

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

;; Easy dependency management for Emacs!

;;; Code:

(eval-and-compile
  (defconst cask-directory
    (file-name-directory
     (cond
      (load-in-progress load-file-name)
      ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
       byte-compile-current-file)
      (:else (buffer-file-name))))
    "Path to Cask root."))

(require 'cask-bootstrap (expand-file-name "cask-bootstrap" cask-directory))

(require 'f)
(require 's)
(require 'dash)
(require 'epl)
(require 'cl-lib)

(eval-and-compile
  (defun cask-resource-path (name)
    "Get the path of a Cask resource with NAME."
    (f-expand name cask-directory)))

(eval-and-compile
  (unless (fboundp 'define-error)
    ;; Shamelessly copied from Emacs trunk :)
    (defun define-error (name message &optional parent)
      "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
      (unless parent (setq parent 'error))
      (let ((conditions
             (if (consp parent)
                 (apply #'nconc
                        (mapcar (lambda (parent)
                                  (cons parent
                                        (or (get parent 'error-conditions)
                                            (error "Unknown signal `%s'" parent))))
                                parent))
               (cons parent (get parent 'error-conditions)))))
        (put name 'error-conditions
             (delete-dups (copy-sequence (cons name conditions))))
        (when message (put name 'error-message message))))))

(define-error 'cask-error "Cask error")
(define-error 'cask-missing-dependencies "Missing dependencies" 'cask-error)
(define-error 'cask-failed-installation "Failed installation" 'cask-error)

(cl-defstruct cask-package name version description)
(cl-defstruct cask-dependency name version)
(cl-defstruct cask-source name url)
(cl-defstruct cask-source-position line column)

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

(defvar cask-project-path nil
  "Path to project.")

(defvar cask-file nil
  "Path to `Cask` file.")

(defvar cask-package-file nil
  "Path to project package (`-pkg.el`) file.")

(defvar cask-development-dependencies nil
  "List of development dependencies.")

(defvar cask-runtime-dependencies nil
  "List of runtime dependencies.")

(defvar cask-package nil
  "Project package information.")

(defvar cask-source-mapping
  '((gnu         . "http://elpa.gnu.org/packages/")
    (melpa       . "http://melpa.milkbox.net/packages/")
    (marmalade   . "http://marmalade-repo.org/packages/")
    (SC          . "http://joseito.republika.pl/sunrise-commander/")
    (org         . "http://orgmode.org/elpa/")
    (cask-test   . "http://127.0.0.1:9191/packages/"))
  "Mapping of source name and url.")

(defun cask-current-source-position ()
  "Get the current position in the buffer."
  (make-cask-source-position :line (line-number-at-pos)
                             :column (1+ (current-column))))

(defun cask-read (filename)
  "Read a cask file from FILENAME.

Return all directives in the Cask file as list."
  (with-temp-buffer
    (insert (f-read-text filename 'utf-8))
    (goto-char (point-min))
    (let (forms)
      (condition-case err
          ;; Skip over blank lines and comments while reading to get exact
          ;; line/column information for errors (shamelessly taken from
          ;; `byte-compile-from-buffer')
          (while (progn
                   (while (progn (skip-chars-forward " \t\n\^l")
                                 (looking-at ";"))
                     (forward-line 1))
                   (not (eobp)))        ; Read until end of file
            (push (read (current-buffer)) forms))
        (error
         ;; Re-emit any error with position information
         (signal (car err) (cons (cask-current-source-position)
                                 (cdr err)))))
      (nreverse forms))))

(defun cask-get-dep-list-for-scope (scope)
  "Get the dependency list symbol for SCOPE."
  (if (eq scope :development)
      'cask-development-dependencies
    'cask-runtime-dependencies))

(defun cask-add-dependency (name &optional version scope)
  "Add the dependency NAME with VERSION in SCOPE."
  (let* ((name (if (stringp name) (intern name) name))
         (dependency (make-cask-dependency :name name :version version))
         (dep-list (cask-get-dep-list-for-scope scope)))
    (add-to-list dep-list dependency t)))

(defun cask-parse-epl-package (package)
  "Parse an EPL PACKAGE."
  (setq cask-package
        (make-cask-package :name (symbol-name (epl-package-name package))
                           :version (epl-package-version-string package)
                           :description (epl-package-summary package)))
  (cl-dolist (req (epl-package-requirements package))
    (cask-add-dependency (epl-requirement-name req)
                         (epl-requirement-version-string req))))

(defun cask-eval (forms &optional scope)
  "Evaluate cask FORMS in SCOPE.

SCOPE may be nil or :development."
  (cl-dolist (form forms)
    (cl-case (car form)
      (source
       (cl-destructuring-bind (_ name-or-alias &optional url) form
         (unless url
           (let ((mapping (assq name-or-alias cask-source-mapping)))
             (unless mapping
               (error "Unknown package archive: %s" name-or-alias))
             (setq name-or-alias (symbol-name (car mapping)))
             (setq url (cdr mapping))))
         (epl-add-archive name-or-alias url)))
      (package
       (cl-destructuring-bind (_ name version description) form
         (setq cask-package (make-cask-package :name name
                                               :version version
                                               :description description))))
      (package-file
       (cl-destructuring-bind (_ filename) form
         (cask-parse-epl-package
          (epl-package-from-file
           (f-expand filename cask-project-path)))))
      (depends-on
       (cl-destructuring-bind (_ name &optional version) form
         (cask-add-dependency name version scope)))
      (development
       (cl-destructuring-bind (_ . body) form
         (cask-eval body :development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun cask-elpa-dir ()
  "Return full path to `cask-project-path'/.cask/`emacs-version'/elpa."
  (f-expand (format ".cask/%s/elpa" emacs-version) cask-project-path))

(defun cask-setup-project-variables (project-path)
  "Setup cask variables for project at PROJECT-PATH."
  (setq cask-project-path project-path)
  (setq cask-file (f-expand cask-filename cask-project-path)))

(defun cask-setup (project-path)
  "Setup cask for project at PROJECT-PATH."
  (cask-setup-project-variables project-path)
  (when (f-same? (epl-package-dir) (epl-default-package-dir))
    (epl-change-package-dir (cask-elpa-dir)))
  (unless (f-file? cask-file)
    (error "Could not locate `Cask` file"))
  (setq package-archives nil)
  (cask-eval (cask-read cask-file))
  (when cask-package
    (let ((package-name (concat (cask-package-name cask-package) "-pkg.el")))
      (setq cask-package-file (f-expand package-name cask-project-path)))))

(defun cask-initialize (&optional project-path)
  "Initialize packages under PROJECT-PATH (defaults to `user-emacs-directory').
Setup `package-user-dir' appropriately and then call `package-initialize'."
  (cask-setup (or project-path user-emacs-directory))
  (epl-initialize))

(defun cask--template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (cask-resource-path "templates"))
         (template-file (f-expand name templates-dir)))
    (f-read-text template-file 'utf-8)))

(defun cask-update ()
  "Update dependencies.

Return a list of updated packages."
  (epl-refresh)
  (epl-initialize)
  (epl-upgrade))

(defun cask-install ()
  "Install dependencies.

Install all available dependencies.

If some dependencies are not available, signal a
`cask-missing-dependencies' error, whose data is a list of all
missing dependencies.  All available dependencies are installed
nonetheless.

If a dependency failed to install, signal a
`cask-failed-installation' error, whose data is a `(DEPENDENCY
. ERR)', where DEPENDENCY is the `cask-dependency' which failed
to install, and ERR is the original error data."
  (let ((cask-dependencies (append cask-development-dependencies cask-runtime-dependencies))
        missing-dependencies)
    (when cask-dependencies
      (epl-refresh)
      (epl-initialize)
      (cl-dolist (dependency cask-dependencies)
        (let ((name (cask-dependency-name dependency)))
          (unless (epl-package-installed-p name)
            (let ((package (car (epl-find-available-packages name))))
              (if package
                  (condition-case err
                      (epl-package-install package)
                    (error (signal 'cask-failed-installation (cons dependency err))))
                (push dependency missing-dependencies))))))
      (when missing-dependencies
        (signal 'cask-missing-dependencies (nreverse missing-dependencies))))))

(defun cask-new-project (project-path &optional dev-mode)
  "Create new project at PROJECT-PATH with optional DEV-MODE."
  (let ((init-content
         (cask--template-get
          (if dev-mode "init-dev.tpl" "init.tpl"))))
    (cask-setup-project-variables project-path)
    (if (f-file? cask-file)
        (error "Cask file already exists.")
      (f-write-text init-content 'utf-8 cask-file))))

(defmacro with-cask-package (&rest body)
  `(if cask-package
       (progn ,@body)
     (error "Missing `package` or `package-file` directive")))

(defun cask-info ()
  "Return info about this project."
  (with-cask-package cask-package))

(defun cask-version ()
  "Return the version of this project."
  (with-cask-package (cask-package-version cask-package)))

(defun cask-package ()
  "Package this project."
  (with-cask-package (cask-define-package-string)))

(defun cask-load-path ()
  "Return Emacs `load-path' (including package dependencies)."
  (let ((dirs (when (f-dir? (cask-elpa-dir))
                (f-directories (cask-elpa-dir)))))
    (s-join path-separator (append dirs load-path))))

(defun cask-path ()
  "Return Emacs `exec-path' (including package dependencies)."
  (s-join path-separator (append (f-glob "*/bin" (cask-elpa-dir)) exec-path)))

(defun cask-define-package-string ()
  "Return `define-package' string."
  (format
   "(define-package \"%s\" \"%s\"\n  \"%s\"%s)\n"
   (cask-package-name cask-package)
   (cask-package-version cask-package)
   (cask-package-description cask-package)
   (let ((dependency-string (cask-dependency-string)))
     (if (equal dependency-string "")
         "" (format "\n  '(%s)" dependency-string)))))

(defun cask-dependency-string ()
  "Return dependencies as string."
  (mapconcat
   (lambda (package)
     (let ((name (cask-dependency-name package))
           (version (cask-dependency-version package)))
       (format "(%s \"%s\")" name (or version ""))))
   cask-runtime-dependencies " "))

(defun cask-outdated ()
  "Return list of `epl-upgrade' objects for outdated packages."
  (epl-refresh)
  (epl-initialize)
  (epl-find-upgrades))

(provide 'cask)

;;; cask.el ends here
