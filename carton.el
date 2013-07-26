;;; carton.el --- Emacs dependency management made easy

;; Copyright (C) 2012, 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner
;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.3.1
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

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (defconst carton-directory
    ;; Fall back to buffer file name to handle M-x eval-buffer
    (file-name-directory (or (and (boundp 'byte-compile-current-file)
                                  byte-compile-current-file)
                             (if load-in-progress
                                 load-file-name
                               (buffer-file-name))))
    "The directory to which Carton is installed.")

  (defun carton-resource-path (name)
    "Get the path of a Carton resource with NAME."
    (expand-file-name name carton-directory)))

(require 'epl (carton-resource-path "epl"))

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

(defvar carton-development-dependencies nil
  "List of development dependencies.")

(defvar carton-runtime-dependencies nil
  "List of runtime dependencies.")

(defvar carton-package nil
  "Project package information.")

(defvar carton-source-mapping
  '((gnu         . "http://elpa.gnu.org/packages/")
    (melpa       . "http://melpa.milkbox.net/packages/")
    (marmalade   . "http://marmalade-repo.org/packages/")
    (SC          . "http://joseito.republika.pl/sunrise-commander/")
    (org         . "http://orgmode.org/elpa/")
    (carton-test . "http://127.0.0.1:9191/packages/"))
  "Mapping of source name and url.")

(defun carton-read (filename)
  "Read a carton file from FILENAME.

Return all directives in the Carton file as list."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (read (format "(%s)" (buffer-string)))))

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

(defun carton-parse-epl-package (package)
  "Parse an EPL PACKAGE."
  (setq carton-package
        (make-carton-package :name (epl-package-name package)
                             :version (epl-package-version-string package)
                             :description (epl-package-summary package)))
  (dolist (req (epl-package-requirements package))
    (carton-add-dependency (epl-requirement-name req)
                             (epl-requirement-version-string req))))

(defun carton-eval (forms &optional scope)
  "Evaluate carton FORMS in SCOPE.

SCOPE may be nil or :development."
  (dolist (form forms)
    (case (car form)
      (source
       (destructuring-bind (_ name-or-alias &optional url) form
         (unless url
           (let ((mapping (assq (car (cdr name-or-alias)) carton-source-mapping)))
             (setq name-or-alias (symbol-name (car mapping)))
             (setq url (cdr mapping))))
         (epl-add-archive name-or-alias url)))
      (package
       (destructuring-bind (_ name version description) form
         (setq carton-package (make-carton-package :name name
                                                   :version version
                                                   :description description))))
      (package-file
       (destructuring-bind (_ filename) form
         (carton-parse-epl-package
          (epl-package-from-file
           (expand-file-name filename carton-project-path)))))
      (depends-on
       (destructuring-bind (_ name &optional version) form
         (carton-add-dependency name version scope)))
      (development
       (destructuring-bind (_ . body) form
         (carton-eval body :development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun carton-elpa-dir ()
  "Return full path to `carton-project-path'/.carton/elpa/`emacs-version'."
  (expand-file-name (format ".carton/%s/elpa" emacs-version)
                    carton-project-path))

(defun carton-setup (project-path)
  "Setup carton for project at PROJECT-PATH."
  (setq carton-project-path (directory-file-name project-path))
  (setq carton-project-name (file-name-nondirectory carton-project-path))
  (setq carton-file (expand-file-name "Carton" carton-project-path))
  (setq carton-package-file (expand-file-name (concat carton-project-name "-pkg.el") carton-project-path))
  (when (equal (epl-package-dir) (epl-default-package-dir))
    (epl-change-package-dir (carton-elpa-dir)))
  (unless (file-exists-p carton-file)
    (error "Could not locate `Carton` file"))
  (carton-eval (carton-read carton-file)))

(defun carton-initialize ()
  "Initialize packages under \"~/.emacs.d/\".
Setup `package-user-dir' appropriately and then call `package-initialize'."
  (carton-setup user-emacs-directory)
  (epl-initialize))

(defun carton--template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (carton-resource-path "templates"))
         (template-file (expand-file-name name templates-dir)))
    (with-temp-buffer
      (insert-file-contents-literally template-file)
      (buffer-string))))

(defun carton-update ()
  "Update dependencies.

Return a list of updated packages."
  (when (< emacs-major-version 24)
    (error "The `update` command is not supported until Emacs 24."))
  (epl-refresh)
  (epl-initialize)
  (epl-upgrade))

(defun carton-install ()
  "Install dependencies."
  (let ((carton-dependencies (append carton-development-dependencies carton-runtime-dependencies)))
    (when carton-dependencies
      (epl-refresh)
      (epl-initialize)
      (dolist (dependency carton-dependencies)
        (epl-package-install (carton-dependency-name dependency))))))

(defun carton-init (path &optional dev-mode)
  "Create new project at PATH with optional DEV-MODE."
  (let ((init-content
         (carton--template-get
          (if dev-mode "init-dev.tpl" "init.tpl")))
        (carton-file-path (expand-file-name "Carton" path)))
    (if (file-exists-p carton-file-path)
        (error "Carton file already exists.")
      (with-temp-buffer
        (insert init-content)
        (write-file carton-file-path)))))

(defun carton-info ()
  "Return info about this project."
  (or
   carton-package
   (error "Missing `package` or `package-file` directive")))

(defun carton-version ()
  "Return the version of this project."
  (if carton-package
      (carton-package-version carton-package)
    (error "Missing `package` or `package-file` directive")))

(defun carton-package ()
  "Package this project."
  (if carton-package
      (carton-define-package-string)
    (error "Missing `package` or `package-file` directive")))

(defun carton-load-path ()
  "Return Emacs load-path (including package dependencies)."
  (mapconcat
   'identity
   (append
    (file-expand-wildcards (concat (carton-elpa-dir) "/*") t)
    load-path)
   path-separator))

(defun carton-path ()
  "Return Emacs exec-path (including package dependencies)."
  (mapconcat
   'identity
   (append
    (file-expand-wildcards (concat (carton-elpa-dir) "/*/bin") t)
    exec-path)
   path-separator))

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
