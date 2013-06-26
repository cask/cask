;;; carton.el --- Emacs dependency management made easy

;; Copyright (C) 2012, 2013 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner

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

(unless (require 'package nil t)
  (require 'package (expand-file-name "carton-package.el"
                                      (file-name-directory load-file-name))))

(defstruct carton-package name version description)
(defstruct carton-dependency name version)
(defstruct carton-source name url)
(defstruct carton-upgrade name old-version new-version)

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

(defun carton-parse-package-info (info)
  "Parse package INFO as returned by `package-buffer-info'."
  (destructuring-bind (name requires description version _)
      ;; We have to convert the INFO vector to a list first, because
      ;; `destructuring-bind' does not support pattern matching on vectors.
      (append info nil)
    (setq carton-package (make-carton-package :name name
                                              :version version
                                              :description description))
    (dolist (req requires)
      (destructuring-bind (name version) req
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
    (case (car form)
      (source
       (destructuring-bind (_ name url) form
         (add-to-list 'package-archives (cons name url))))
      (package
       (destructuring-bind (_ name version description) form
         (setq carton-package (make-carton-package :name name
                                                   :version version
                                                   :description description))))
      (package-file
       (destructuring-bind (_ filename) form
         (carton-parse-package-file filename)))
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
  (when (equal (eval (car (get 'package-user-dir 'standard-value))) package-user-dir)
    (setq package-user-dir (carton-elpa-dir)))
  (unless (file-exists-p carton-file)
    (error "Could not locate `Carton` file"))
  (carton-eval (carton-read carton-file)))

(defun carton-initialize ()
  "Initialize packages under \"~/.emacs.d/\".
Setup `package-user-dir' appropriately and then call `package-initialize'."
  (carton-setup user-emacs-directory)
  (package-initialize))

(defun carton--template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (expand-file-name "templates" (file-name-directory load-file-name)))
         (template-file (expand-file-name name templates-dir)))
    (with-temp-buffer
      (insert-file-contents-literally template-file)
      (buffer-string))))

(defun carton-update ()
  "Update dependencies.

Return a list of updated packages."
  (with-temp-buffer
    (package-refresh-contents)
    (package-initialize)
    (package-menu--generate nil t) ;; WTF ELPA, really???
    (let ((upgrades (package-menu--find-upgrades))
          installed-upgrades)
      (dolist (upgrade upgrades)
        (let* ((name (car upgrade))
               (new-version (cdr upgrade))
               (old-version (package-desc-vers (cdr (assq name package-alist))))
               (upgrade (make-carton-upgrade :name name
                                             :old-version old-version
                                             :new-version new-version)))
          (package-install name)
          (push upgrade installed-upgrades)))
      ;; Delete obsolete packages
      (dolist (pkg package-obsolete-alist)
         (package-delete (symbol-name (car pkg))
                         (package-version-join (caadr pkg))))
      (reverse installed-upgrades))))

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

(defun carton-exec (arguments)
  "Execute command with correct load and exec path."
  (let* ((load-path
          (append
           (if (file-exists-p (carton-elpa-dir))
               (directory-files (carton-elpa-dir) t "^.+-[^-]+$"))
           load-path))
         (buffer (get-buffer-create "*carton*"))
         (command
          (or
           (executable-find (car arguments))
           (expand-file-name (car arguments) default-directory)))
         (args (cdr arguments)))
    (let* ((process-environment (cons (concat "EMACSLOADPATH=" (mapconcat 'identity load-path ":")) process-environment))
           (exit-code (apply 'call-process (append (list command nil buffer nil) args))))
      (with-current-buffer buffer
        (princ (buffer-string)))
      (kill-emacs exit-code))))

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
