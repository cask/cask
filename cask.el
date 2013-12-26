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
(define-error 'cask-not-a-package "Missing `package` or `package-file` directive" 'cask-error)
(define-error 'cask-no-cask-file "Cask file does not exist" 'cask-error)

(cl-defstruct cask-dependency name version)
(cl-defstruct cask-source name url)
(cl-defstruct cask-bundle name version description dependencies path)
(cl-defstruct cask-source-position line column)

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

;; TODO: Remove
(defvar cask-file nil
  "Path to `Cask` file.")

;; Do not trust the value of these variables externally, they should
;; only be used by Cask itself. The same information is externally
;; available from the Cask API.
(defvar cask-package nil)
(defvar cask-runtime-dependencies nil)
(defvar cask-development-dependencies nil)

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
        (list :name (symbol-name (epl-package-name package))
              :version (epl-package-version-string package)
              :description (epl-package-summary package)))
  (cl-dolist (req (epl-package-requirements package))
    (cask-add-dependency (epl-requirement-name req)
                         (epl-requirement-version-string req))))

(defun cask-eval (bundle forms &optional scope)
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
         (setq cask-package (list :name name
                                  :version version
                                  :description description))))
      (package-file
       (cl-destructuring-bind (_ filename) form
         (cask-parse-epl-package
          (epl-package-from-file
           (f-expand filename (cask-bundle-path bundle))))))
      (depends-on
       (cl-destructuring-bind (_ name &optional version) form
         (cask-add-dependency name version scope)))
      (development
       (cl-destructuring-bind (_ . body) form
         (cask-eval bundle body :development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun cask-elpa-dir (bundle)
  "Return full path to BUNDLE elpa directory."
  (f-expand (format ".cask/%s/elpa" emacs-version) (cask-bundle-path bundle)))

(defun cask-setup-project-variables (project-path)
  "Setup cask variables for project at PROJECT-PATH."
  (setq cask-file (f-expand cask-filename project-path)))

(defun cask-setup (project-path)
  "Setup cask for project at PROJECT-PATH."
  (let ((bundle (make-cask-bundle :path project-path)))
    (cask-setup-project-variables project-path)
    (when (f-same? (epl-package-dir) (epl-default-package-dir))
      (epl-change-package-dir (cask-elpa-dir bundle)))
    (setq package-archives nil)
    (let (cask-package cask-runtime-dependencies cask-development-dependencies)
      (when (f-file? cask-file)
        (cask-eval bundle (cask-read cask-file)))
      (setf (cask-bundle-dependencies bundle)
            (list :runtime cask-runtime-dependencies
                  :development cask-development-dependencies))
      (when cask-package
        (setf (cask-bundle-name bundle) (plist-get cask-package :name))
        (setf (cask-bundle-version bundle) (plist-get cask-package :version))
        (setf (cask-bundle-description bundle) (plist-get cask-package :description))))
    bundle))

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

(defun cask-caskify (path &optional dev-mode)
  "Create Cask-file in PATH.

If DEV-MODE is true, the dev template is used, otherwise the
configuration template is used."
  (let ((init-content
         (cask--template-get (if dev-mode "init-dev.tpl" "init.tpl")))
        (cask-file (f-expand "Cask" path)))
    (if (f-file? cask-file)
        (error "Cask file already exists.")
      (f-write-text init-content 'utf-8 cask-file))))

(put 'with-cask-file 'lisp-indent-function 2)
(defmacro with-cask-file (bundle &rest body)
  "If BUNDLE path has a Cask-file, yield BODY.

If BUNDLE is not a package, the error `cask-no-cask-file' is signaled."
  `(if (f-file? cask-file)
       (progn ,@body)
     (signal 'cask-no-cask-file (list cask-file))))

(put 'with-cask-package 'lisp-indent-function 2)
(defmacro with-cask-package (bundle &rest body)
  "If BUNDLE is a package, yield BODY.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  `(with-cask-file bundle
       (if (and
            (cask-bundle-name bundle)
            (cask-bundle-version bundle)
            (cask-bundle-description bundle))
           (progn ,@body)
         (signal 'cask-not-a-package nil))))

(defun cask-package-name (bundle)
  "Return BUNDLE name.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (with-cask-package bundle (cask-bundle-name bundle)))

(defun cask-package-version (bundle)
  "Return BUNDLE version.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (with-cask-package bundle (cask-bundle-version bundle)))

(defun cask-package-description (bundle)
  "Return BUNDLE description.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (with-cask-package bundle (cask-bundle-description bundle)))

(defun cask-version ()
  "Return Cask's version."
  (let ((package (epl-package-from-lisp-file
                  (f-expand "cask.el" cask-directory))))
    (epl-package-version-string package)))

(defun cask-load-path (bundle)
  "Return Emacs `load-path' (including BUNDLE dependencies)."
  (let ((dirs (when (f-dir? (cask-elpa-dir bundle))
                (f-directories (cask-elpa-dir bundle)))))
    (s-join path-separator (append dirs load-path))))

(defun cask-exec-path (bundle)
  "Return Emacs `exec-path' (including BUNDLE dependencies)."
  (s-join path-separator (append (f-glob "*/bin" (cask-elpa-dir bundle)) exec-path)))

(defun cask-runtime-dependencies (bundle)
  "Return BUNDLE's runtime dependencies.

Return value is a list of `cask-dependency' objects."
  (with-cask-file bundle
      (plist-get (cask-bundle-dependencies bundle) :runtime)))

(defun cask-development-dependencies (bundle)
  "Return BUNDLE's development dependencies.

Return value is a list of `cask-dependency' objects."
  (with-cask-file bundle
      (plist-get (cask-bundle-dependencies bundle) :development)))

(defun cask-define-package-string (bundle)
  "Return `define-package' string for BUNDLE."
  (with-cask-package bundle
      (let ((name (cask-bundle-name bundle))
            (version (cask-bundle-version bundle))
            (description (cask-bundle-description bundle))
            (dependencies
             (-map
              (lambda (dependency)
                (list (cask-dependency-name dependency)
                      (cask-dependency-version dependency)))
              (cask-runtime-dependencies bundle))))
        (pp-to-string `(define-package ,name ,version ,description ',dependencies)))))

(defun cask-define-package-file (bundle)
  "Return path to `define-package' file for BUNDLE."
  (with-cask-package bundle
      (f-expand (concat (cask-bundle-name bundle) "-pkg.el") (cask-bundle-path bundle))))

(defun cask-outdated ()
  "Return list of `epl-upgrade' objects for outdated packages."
  (epl-refresh)
  (epl-initialize)
  (epl-find-upgrades))

(defun cask-path (bundle)
  "Return BUNDLE root path."
  (cask-bundle-path bundle))

(defun cask-file (bundle)
  "Return path to BUNDLE Cask-file."
  (f-expand "Cask" (cask-path bundle)))

(provide 'cask)

;;; cask.el ends here
