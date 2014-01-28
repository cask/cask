;;; cask.el --- Cask: Emacs dependency management made easy  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2014 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner
;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.5.1
;; Keywords: speed, convenience
;; URL: http://github.com/cask/cask
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.16.0") (epl "0.5") (shut-up "0.1.0") (cl-lib "0.3") (package-build "0.1"))

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

(setq package-build-verbose nil)        ; Make package-build quiet by
                                        ; not allowing it to print any
                                        ; messages.

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
(define-error 'cask-failed-initialization "Failed initialization" 'cask-error)
(define-error 'cask-not-a-package "Missing `package` or `package-file` directive" 'cask-error)
(define-error 'cask-no-cask-file "Cask file does not exist" 'cask-error)

(cl-defstruct cask-dependency
  "Structure representing a dependency.

Slots:

`name' The package name, as symbol.

`version' The version of the dependency package, as version string."
  name version)

(cl-defstruct cask-source
  "Structure representing a package source.

Slots:

`name' Name of the source, as string.

`url' Package source url."
  name url)

(cl-defstruct cask-bundle
  "Structure for a Cask project. All information necessary to
  describe the current project is in this structure. Most public
  API functions take this structure as argument.

Slots:

`name' Name of the package, as symbol.

`version' The version of the dependency package, as version string.

`description' Package description, as string.

`runtime-dependencies' Package runtime dependencies.

`development-dependencies' Package development dependencies. The
package does not require these dependencies itself. These are
dependencies that are required for local development.

`path' Path to project root directory.

`patterns' List of files patterns.

`sources' List of `cask-source' objects."
  name version description runtime-dependencies
  development-dependencies path patterns sources)

(cl-defstruct cask-source-position
  "Structure for a position in a Cask-file.

Slots:

`line' Line number in the file.

`column' Column number on the `line'."
  line column)

(defvar cask-source-mapping
  '((gnu         . "http://elpa.gnu.org/packages/")
    (melpa       . "http://melpa.milkbox.net/packages/")
    (marmalade   . "http://marmalade-repo.org/packages/")
    (SC          . "http://joseito.republika.pl/sunrise-commander/")
    (org         . "http://orgmode.org/elpa/"))
  "Mapping of source name and url.")

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

(defconst cask-dist-path "dist"
  "Name of default target directory for building packages.")

(defvar cask-current-bundle nil
  "Cache the currently used bundle environment.

This variable should not be modifed.  It is used by
the function `cask--with-environment'.")


;;;; Internal functions

(defun cask--find-unbalanced-parenthesis (bundle)
  "Find unbalanced parenthesis for Cask file in BUNDLE."
  (with-temp-buffer
    (insert (f-read-text (cask-file bundle) 'utf-8))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (check-parens)
          nil)
      (error (cask--current-source-position)))))

(defun cask--exit-error (bundle err)
  "Exit with improved error messages specified from BUNDLE.

ERR is the error object."
  (let ((type (car err))
        (data (cdr err))
        pos msg)
    (if (eq type 'end-of-file)
        ;; In case of premature end of file, try hard to find the real
        ;; position, by scanning for unbalanced parenthesis
        (setq pos (or (cask--find-unbalanced-parenthesis bundle) (cadr err))
              msg "End of file while reading (possible unbalanced parenthesis)")
      ;; For other types of error, check whether the error has a position, and
      ;; print it.  Otherwise just print the error like Emacs would do
      (when (cask-source-position-p (car data))
        (setq pos (car data))
        ;; Strip the position from the error data
        (setq data (cdr data)))
      (setq msg (error-message-string (cons type data))))
    (if pos
        (message "%s:%s:%s: %s" (cask-file bundle) (cask-source-position-line pos)
                 (cask-source-position-column pos) msg)
      (message "%s: %s" (cask-file bundle) msg)))
  (kill-emacs 1))

(defun cask--upgradable-dependencies (bundle)
  "Return list of upgradable dependencies for BUNDLE.

A dependency is upgradable if it if a dependency (non deep) and
is not currently linked."
  (-reject
   (lambda (dependency)
     (cask-linked-p bundle (cask-dependency-name dependency)))
   (cask-dependencies bundle)))

(defun cask--upgradable-dependencies-as-epl-packages (bundle)
  "Return a list of upgradable packages for BUNDLE.

The list contains `epl-package' objects."
  (-map
   (lambda (dependency)
     (epl-find-installed-package (cask-dependency-name dependency)))
   (cask--upgradable-dependencies bundle)))

(defun cask--current-source-position ()
  "Get the current position in the buffer."
  (make-cask-source-position :line (line-number-at-pos)
                             :column (1+ (current-column))))

(defun cask--read (filename)
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
         (signal (car err) (cons (cask--current-source-position)
                                 (cdr err)))))
      (nreverse forms))))

(defun cask--use-environment (bundle &optional refresh)
  "Use BUNDLE environment.

If REFRESH is true, refresh the environment by fetching new data
from the sources."
  (cask--with-file bundle
    (setq package-archives nil)
    (epl-change-package-dir (cask-elpa-path bundle))
    (-each (cask-bundle-sources bundle)
      (lambda (source)
        (epl-add-archive (cask-source-name source)
                         (cask-source-url source))))
    (shut-up
     (condition-case err
         (progn
           (when refresh
             (epl-refresh))
           (epl-initialize))
       (error
        (signal 'cask-failed-initialization
                (list err (shut-up-current-output))))))))

(defmacro cask--with-environment (bundle &rest body)
  "Switch to BUNDLE environment and yield BODY.

This function will not switch to the bundle if it's already the
currently used bundle environment.  If :force property is present
in BODY and true, the environment will be reinitalized.

If :refresh property is present in BODY, it will be passed as
refresh argument to `cask--use-environment'.

When BODY has yielded, this function cleans up side effects
outside of package.el, for example `load-path'."
  (declare (indent defun))
  `(cask--with-file bundle
     (when (or ,(plist-get body :force) (not (equal ,bundle cask-current-bundle)))
       (let ((load-path (-clone load-path)))
         (cask--use-environment ,bundle ,(plist-get body :refresh)))
       (setq cask-current-bundle (copy-cask-bundle ,bundle)))
     ,@body))

(defmacro cask--with-file (bundle &rest body)
  "If BUNDLE path has a Cask-file, yield BODY.

If BUNDLE is not a package, the error `cask-no-cask-file' is signaled."
  (declare (indent 1))
  `(if (f-file? (cask-file ,bundle))
       (progn ,@body)
     (signal 'cask-no-cask-file (list (cask-file ,bundle)))))

(defmacro cask--with-package (bundle &rest body)
  "If BUNDLE is a package, yield BODY.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (declare (indent 1))
  `(cask--with-file bundle
     (if (and
          (cask-bundle-name ,bundle)
          (cask-bundle-version ,bundle)
          (cask-bundle-description ,bundle))
         (progn ,@body)
       (signal 'cask-not-a-package nil))))

(defun cask--eval (bundle forms &optional scope)
  "Populare BUNDLE by evaluating FORMS in SCOPE.

SCOPE may be nil or :development."
  (cl-dolist (form forms)
    (cl-case (car form)
      (source
       (cl-destructuring-bind (_ name-or-alias &optional url) form
         (cask-add-source bundle name-or-alias url)))
      (package
       (cl-destructuring-bind (_ name version description) form
         (setf (cask-bundle-name bundle) (intern name))
         (setf (cask-bundle-version bundle) version)
         (setf (cask-bundle-description bundle) description)))
      (package-file
       (cl-destructuring-bind (_ filename) form
         (let ((package (epl-package-from-file
                         (f-expand filename (cask-bundle-path bundle)))))
           (setf (cask-bundle-name bundle) (epl-package-name package))
           (setf (cask-bundle-version bundle) (epl-package-version-string package))
           (setf (cask-bundle-description bundle) (epl-package-summary package))
           (-each (epl-package-requirements package)
             (lambda (requirement)
               (cask-add-dependency bundle
                                    (epl-requirement-name requirement)
                                    (epl-requirement-version-string requirement)))))))
      (depends-on
       (cl-destructuring-bind (_ name &optional version) form
         (cask-add-dependency bundle name version scope)))
      (files
       (cl-destructuring-bind (_ &rest patterns) form
         (setf (cask-bundle-patterns bundle) patterns)))
      (development
       (cl-destructuring-bind (_ . body) form
         (cask--eval bundle body :development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun cask--template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (cask-resource-path "templates"))
         (template-file (f-expand name templates-dir)))
    (f-read-text template-file 'utf-8)))

(defun cask--initialized-p (bundle)
  "Return true if BUNDLE is initialized.

The BUNDLE is initialized when the elpa directory exists."
  (f-dir? (cask-elpa-path bundle)))

(defun cask--dependency-to-string (dependency)
  "Return string representatation of DEPENDENCY."
  (let ((name (symbol-name (cask-dependency-name dependency)))
        (version (cask-dependency-version dependency)))
    (s-join "-" (list name version))))

(defun cask--installed-dependencies-paths (bundle)
  "Return list of paths for all installed BUNDLE dependencies."
  (--map
   (cask-dependency-path bundle (cask-dependency-name it))
   (cask--installed-dependencies bundle 'deep)))

(defun cask--epl-package-to-dependency (epl-package)
  "Turn EPL-PACKAGE into a `cask-dependency' object."
  (make-cask-dependency
   :name (epl-package-name epl-package)
   :version (epl-package-version-string epl-package)))

(defun cask--dependency-installed-p (bundle name)
  "Return true if BUNDLE has and installed dependency with NAME.

This is the Cask definition if a dependency is installed or not.
The functions in package.el requires a package to have a -pkg.el
file to be considered installed.  This does not work in Cask
because we allow links that may not have a -pkg.el file.

The dependency is considered installed if there exists a
dependency for NAME and if there exists a directory or link for
NAME."
  (and
   (-any?
    (lambda (dependency)
      (eq (cask-dependency-name dependency) name))
    (cask-dependencies bundle 'deep))
   (let ((package-path (cask-dependency-path bundle name)))
     (and package-path (f-dir? package-path)))))

(defun cask--find-installed-package (bundle name)
  "Return installed package in BUNDLE with NAME.

This function is similar to `epl-find-installed-package', but
uses `cask--dependency-installed-p' to determine if the dependency
is installed or not."
  (-when-let (package (car (epl-find-available-packages name)))
    (when (cask--dependency-installed-p bundle name)
      package)))

(defun cask--dependency-dependencies (name)
  "Return a list of all NAME's dependencies, recursively."
  (-when-let (package (car (epl-find-available-packages name)))
    (cons
     (cask--epl-package-to-dependency package)
     (-flatten
      (--map (cask--dependency-dependencies (epl-requirement-name it))
             (epl-package-requirements package))))))

(defun cask--installed-dependency-dependencies (bundle name)
  "Return a list of installed BUNDLE dependencies for NAME, recursively."
  (-when-let (package (cask--find-installed-package bundle name))
    (cons
     (cask--epl-package-to-dependency package)
     (-flatten
      (--map (cask--installed-dependency-dependencies bundle (epl-requirement-name it))
             (epl-package-requirements package))))))

(defun cask--dependencies-deep (dependencies)
  "Return a list of DEPENDENCIES dependencies, recursively."
  (-uniq
   (-flatten
    (-map
     (lambda (dependency)
       (cask--dependency-dependencies (cask-dependency-name dependency)))
     dependencies))))

(defun cask--runtime-dependencies (bundle &optional deep)
  "Return runtime dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask-bundle-runtime-dependencies bundle)))
    (if deep (cask--dependencies-deep dependencies) dependencies)))

(defun cask--development-dependencies (bundle &optional deep)
  "Return development dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask-bundle-development-dependencies bundle)))
    (if deep (cask--dependencies-deep dependencies) dependencies)))

(defun cask--dependencies (bundle &optional deep)
  "Return dependencies for BUNDLE, optionally DEEP."
  (append (cask--runtime-dependencies bundle deep)
          (cask--development-dependencies bundle deep)))

(defun cask--installed-dependencies (bundle &optional deep)
  "Return installed dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask--dependencies bundle)))
    (if deep
        (-uniq
         (-flatten
          (-map
           (lambda (dependency)
             (cask--installed-dependency-dependencies bundle (cask-dependency-name dependency)))
           dependencies)))
      (-select
       (lambda (dependency)
         (cask--dependency-installed-p bundle (cask-dependency-name dependency)))
       dependencies))))


;;;; Public API

(defun cask-setup (project-path)
  "Setup cask for project at PROJECT-PATH.

This function return a `cask-bundle' object."
  (let ((bundle (make-cask-bundle :path (f-canonical project-path))))
    (when (f-file? (cask-file bundle))
      (condition-case err
          (cask--eval bundle (cask--read (cask-file bundle)))
        (end-of-file
         (cask--exit-error bundle err))
        (invalid-read-syntax
         (cask--exit-error bundle err))))
    bundle))

(defun cask-initialize (&optional project-path)
  "Initialize packages under PROJECT-PATH or `user-emacs-directory'.

This function return a `cask-bundle' object."
  (let* ((bundle (cask-setup (or project-path user-emacs-directory)))
         (package-load-list
          (unwind-protect
              (progn
                (epl-change-package-dir (cask-elpa-path bundle))
                (--map (list (cask-dependency-name it)
                             (cask-dependency-version it))
                       (cask--installed-dependencies bundle 'deep)))
            (epl-reset))))
    (when (f-same? (epl-package-dir) (epl-default-package-dir))
      (cask--use-environment bundle))
    bundle))

(defun cask-update (bundle)
  "Update BUNDLE dependencies.

Return list of updated packages."
  (cask--with-environment bundle
    :force t
    :refresh t
    (shut-up
     (condition-case err
         (-when-let (packages (cask--upgradable-dependencies-as-epl-packages bundle))
           (epl-upgrade packages))
       (error
        (signal 'cask-failed-installation
                (list nil err (shut-up-current-output))))))))

(defun cask-outdated (bundle)
  "Return list of `epl-upgrade' objects for outdated BUNDLE dependencies."
  (cask--with-environment bundle
    :force t
    :refresh t
    (-when-let (packages (cask--upgradable-dependencies-as-epl-packages bundle))
      (epl-find-upgrades packages))))

(defun cask-install (bundle)
  "Install BUNDLE dependencies.

Install all available dependencies.

If some dependencies are not available, signal a
`cask-missing-dependencies' error, whose data is a list of all
missing dependencies.  All available dependencies are installed
nonetheless.

If a dependency failed to install, signal a
`cask-failed-installation' error, whose data is a `(DEPENDENCY
. ERR)', where DEPENDENCY is the `cask-dependency' which failed
to install, and ERR is the original error data."
  (let (missing-dependencies)
    (cask--with-environment bundle
      :force t
      :refresh t
      (-when-let (dependencies (cask--dependencies bundle))
        (-each dependencies
          (lambda (dependency)
            (let ((name (cask-dependency-name dependency)))
              (unless (and (epl-package-installed-p name) (cask-linked-p bundle name))
                (-if-let (package (car (epl-find-available-packages name)))
                    (shut-up
                     (condition-case err
                         (epl-package-install package)
                       (error
                        (signal 'cask-failed-installation
                                (list dependency err (shut-up-current-output))))))
                  (push dependency missing-dependencies)))))))
      (when missing-dependencies
        (signal 'cask-missing-dependencies (nreverse missing-dependencies))))))

(defun cask-caskify (bundle &optional dev-mode)
  "Create Cask-file for BUNDLE path.

If DEV-MODE is true, the dev template is used, otherwise the
configuration template is used."
  (let ((cask-file (cask-file bundle))
        (init-content
         (cask--template-get (if dev-mode "init-dev.tpl" "init.tpl"))))
    (if (f-file? cask-file)
        (error "Cask-file already exists")
      (f-write-text init-content 'utf-8 cask-file))))

(defun cask-package-name (bundle)
  "Return BUNDLE name.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask--with-package bundle (cask-bundle-name bundle)))

(defun cask-package-version (bundle)
  "Return BUNDLE version.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask--with-package bundle (cask-bundle-version bundle)))

(defun cask-package-description (bundle)
  "Return BUNDLE description.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask--with-package bundle (cask-bundle-description bundle)))

(defun cask-version ()
  "Return Cask's version."
  (let ((package (epl-package-from-lisp-file
                  (f-expand "cask.el" cask-directory))))
    (epl-package-version-string package)))

(defun cask-load-path (bundle)
  "Return Emacs `load-path' (including BUNDLE dependencies)."
  (cask--with-environment bundle
    (append (cask--installed-dependencies-paths bundle) load-path)))

(defun cask-exec-path (bundle)
  "Return Emacs `exec-path' (including BUNDLE dependencies)."
  (cask--with-environment bundle
    (append
     (-select
      'f-dir?
      (--map (f-expand "bin" it)
             (cask--installed-dependencies-paths bundle)))
     exec-path)))

(defun cask-elpa-path (bundle)
  "Return full path to BUNDLE elpa directory."
  (f-expand (format ".cask/%s/elpa" emacs-version) (cask-bundle-path bundle)))

(defun cask-runtime-dependencies (bundle &optional deep)
  "Return BUNDLE's runtime dependencies.

If DEEP is true, return all dependencies, recursively.

Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--runtime-dependencies bundle deep)))

(defun cask-development-dependencies (bundle &optional deep)
  "Return BUNDLE's development dependencies.

If DEEP is true, return all dependencies, recursively.

Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--development-dependencies bundle deep)))

(defun cask-dependencies (bundle &optional deep)
  "Return BUNDLE's runtime and development dependencies.

If DEEP is true, return all dependencies, recursively.

Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--dependencies bundle deep)))

(defun cask-installed-dependencies (bundle &optional deep)
  "Return list of BUNDLE's installed dependencies.

If DEEP is t, all dependencies recursively will be returned."
  (cask--with-environment bundle
    (cask--installed-dependencies bundle deep)))

(defun cask-has-dependency (bundle name)
  "Return true if BUNDLE contain link with NAME, false otherwise."
  (--any? (eq (cask-dependency-name it) name) (cask--dependencies bundle)))

(defun cask-define-package-string (bundle)
  "Return `define-package' string for BUNDLE."
  (cask--with-package bundle
    (let ((name (symbol-name (cask-bundle-name bundle)))
          (version (cask-bundle-version bundle))
          (description (cask-bundle-description bundle))
          (dependencies
           (-map
            (lambda (dependency)
              (list (cask-dependency-name dependency)
                    (cask-dependency-version dependency)))
            (cask--runtime-dependencies bundle))))
      (pp-to-string `(define-package ,name ,version ,description ',dependencies)))))

(defun cask-define-package-file (bundle)
  "Return path to `define-package' file for BUNDLE."
  (cask--with-package bundle
    (f-expand (concat (symbol-name (cask-bundle-name bundle)) "-pkg.el") (cask-bundle-path bundle))))

(defun cask-dependency-path (bundle name)
  "Return path to BUNDLE dependency with NAME.

If no such dependency exist, return nil."
  (car (f-glob (concat (symbol-name name) "-*") (cask-elpa-path bundle))))

(defun cask-path (bundle)
  "Return BUNDLE root path."
  (cask-bundle-path bundle))

(defun cask-file (bundle)
  "Return path to BUNDLE Cask-file."
  (f-expand "Cask" (cask-path bundle)))

(defun cask-files (bundle)
  "Return BUNDLE files list.

This is done by expanding the patterns in the BUNDLE path.  Files
in the list are relative to the path."
  (cask--with-file bundle
    (let ((path (cask-bundle-path bundle))
          (patterns (or (cask-bundle-patterns bundle) package-build-default-files-spec)))
      (-map 'car (ignore-errors (package-build-expand-file-specs path patterns))))))

(defun cask-add-dependency (bundle name version &optional scope)
  "Add to BUNDLE the dependency NAME with VERSION in SCOPE.

SCOPE can be either nil, which means it's a runtime dependency
or `:development', which means it's a development dependency."
  (let ((name (if (stringp name) (intern name) name)))
    (push (make-cask-dependency :name name :version version)
          (if (eq scope :development)
              (cask-bundle-development-dependencies bundle)
            (cask-bundle-runtime-dependencies bundle)))))

(defun cask-add-source (bundle name-or-alias &optional url)
  "Add source to BUNDLE.

NAME-OR-ALIAS is either a string with the name of the source or a
symbol, which refers to some of the keys in
`cask-source-mapping'.

Second argument URL is only required unless alias.  If no alias,
URL is the url to the mirror."
  (cask--with-file bundle
    (unless url
      (let ((mapping (assq name-or-alias cask-source-mapping)))
        (unless mapping
          (error "Unknown package archive: %s" name-or-alias))
        (setq name-or-alias (symbol-name (car mapping)))
        (setq url (cdr mapping))))
    (push (make-cask-source :name name-or-alias :url url) (cask-bundle-sources bundle))))

(defun cask-build (bundle)
  "Build BUNDLE Elisp files."
  (cask--with-file bundle
    (require 'bytecomp)
    (-each (cask-files bundle)
      (lambda (path)
        (when (and (f-file? path) (f-ext? path "el"))
          (byte-recompile-file path 'force 0))))))

(defun cask-clean-elc (bundle)
  "Remove BUNDLE Elisp byte compiled files."
  (cask--with-file bundle
    (-each (cask-files bundle)
      (lambda (path)
        (when (and (f-file? path) (f-ext? path "el"))
          (when (f-file? (concat path "c"))
            (f-delete (concat path "c"))))))))

(defun cask-links (bundle)
  "Return a list of all links for BUNDLE.

The list is a list of alist's where the key is the name of the
link, as a string and the value is the absolute path to the link."
  (cask--with-file bundle
    (when (cask--initialized-p bundle)
      (-map
       (lambda (file)
         (list (f-filename file) (f-canonical file)))
       (f-entries (cask-elpa-path bundle) 'f-symlink?)))))

(defun cask-link (bundle name source)
  "Add BUNDLE link with NAME to SOURCE.

NAME is the name of the package to link as a string.  SOURCE is
the path to the directory to link to.

This will create the link `cask-elpa-path'/NAME-dev pointing to
TARGET."
  (cask--with-file bundle
    (unless (cask-has-dependency bundle name)
      (error "Cannot link package %s, is not a dependency" name))
    (unless (f-dir? source)
      (error "Cannot create link %s to non existing path: %s" name source))
    (when (cask-linked-p bundle name)
      (error "Package %s has already been linked" name))
    (when (cask--initialized-p bundle)
      (let ((target (cask-dependency-path bundle name)))
        (when (and target (f-exists? target))
          (f-delete target 'force))
        (f-symlink source target)))))

(defun cask-link-delete (bundle name)
  "Delete BUNDLE link with NAME."
  (cask--with-file bundle
    (unless (cask-has-dependency bundle name)
      (error "Cannot link package %s, is not a dependency" name))
    (let ((link (cask-dependency-path bundle name)))
      (if (and link (f-symlink? link))
          (progn
            (f-delete link)
            (cask--with-environment bundle
              :force t
              :refresh t
              (epl-package-install (car (epl-find-available-packages name)))))
        (error "Package %s not linked" name)))))

(defun cask-linked-p (bundle name)
  "Return true if BUNDLE has link with NAME."
  (f-symlink? (cask-dependency-path bundle name)))

(defun cask-package (bundle &optional target-dir)
  "Build an Elpa package of BUNDLE.

Put package in TARGET-DIR if specified.  If not specified, put in
a directory specified by `cask-dist-path' in the BUNDLE path."
  (cask--with-package bundle
    (let ((name (symbol-name (cask-bundle-name bundle)))
          (version (cask-bundle-version bundle))
          (patterns (or (cask-bundle-patterns bundle)
                        package-build-default-files-spec))
          (path (cask-bundle-path bundle)))
      (unless target-dir
        (setq target-dir (f-expand cask-dist-path path)))
      (unless (f-dir? target-dir)
        (f-mkdir target-dir))
      (package-build-package name version patterns path target-dir))))

(provide 'cask)

;;; cask.el ends here
