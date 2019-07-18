;;; cask.el --- Cask: Project management for Emacs package development  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2014 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.8.4
;; Keywords: speed, convenience
;; URL: http://github.com/cask/cask
;; Package-Requires: ((s "1.8.0") (dash "2.2.0") (f "0.16.0") (epl "0.5") (shut-up "0.1.0") (cl-lib "0.3") (package-build "1.2") (ansi "0.4.1"))

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

;; Cask is a project management tool for Emacs that helps automate the
;; package development cycle.

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
(define-error 'cask-missing-dependency "Missing dependency" 'cask-error)
(define-error 'cask-missing-dependencies "Missing dependencies" 'cask-error)
(define-error 'cask-failed-installation "Failed installation" 'cask-error)
(define-error 'cask-failed-initialization "Failed initialization" 'cask-error)
(define-error 'cask-not-a-package "Missing `package` or `package-file` directive" 'cask-error)
(define-error 'cask-no-cask-file "Cask file does not exist" 'cask-error)

(cl-defstruct cask-dependency
  "Structure representing a dependency.

Slots:

`name' The package name, as symbol.

`version' The version of the dependency package, as version string.

`fetcher' Name of the fetcher. Available fetchers are specified
by the variable `cask-fetchers'.

`url' Url to the fetcher repository.

`files' Files to include in build.  This property should only be used
when fetcher is specified.

`ref' The ref to use if fetcher.

`branch' The branch to use if fetcher."
  name version fetcher url files ref branch)

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

;; Specializations of package-build classes and methods to define a
;; directory based recipe.
(defclass package-directory-recipe (package-recipe)
  ((dir           :initarg :dir   :initform ".")))

(defmethod package-recipe--working-tree ((rcp package-directory-recipe))
  (oref rcp dir))

(defmethod package-build--get-commit ((rcp package-directory-recipe)))

(defvar cask-source-mapping
  '((gnu          . "https://elpa.gnu.org/packages/")
    (melpa        . "https://melpa.org/packages/")
    (melpa-stable . "https://stable.melpa.org/packages/")
    (marmalade    . "https://marmalade-repo.org/packages/")
    (org          . "http://orgmode.org/elpa/"))
  "Mapping of source name and url.")

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

(defconst cask-dist-path "dist"
  "Name of default target directory for building packages.")

(defvar cask-current-bundle nil
  "Cache the currently used bundle environment.

This variable should not be modifed.  It is used by
the function `cask--with-environment'.")

(defconst cask-fetchers
  '(:git :bzr :hg :darcs :svn :cvs)
  "List of supported fetchers.")

(defconst cask-tmp-path
  (f-expand "cask" temporary-file-directory))

(defconst cask-tmp-checkout-path
  (f-expand "checkout" cask-tmp-path))

(defconst cask-tmp-packages-path
  (f-expand "packages" cask-tmp-path))


;;;; Internal functions

(defmacro cask-print (&rest body)
  "Print messages to `standard-output'.

The body of this macro is automatically wrapped with
`with-ansi' for easier colored output.

If `cask-cli--silent' is non-nil, do not print anything."
  `(when (and (boundp 'cask-cli--silent)
              (not cask-cli--silent))
     (princ
      (with-ansi
       ,@body))))

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

(defmacro cask--with-file (bundle &rest body)
  "If BUNDLE path has a Cask-file, yield BODY.

If BUNDLE is not a package, the error `cask-no-cask-file' is signaled."
  (declare (indent 1))
  `(if (f-file? (cask-file ,bundle))
       (progn ,@body)
     (signal 'cask-no-cask-file (list (cask-file ,bundle)))))

(defun cask--use-environment (bundle &rest args)
  "Use BUNDLE environment.

ARGS is a plist with these additional options:

`refresh' If non nil, refresh the environment by calling `epl-refresh'.

`activate' If non nil, activate packages on initialization."
  (cask--with-file bundle
    (setq package-archives nil)
    (setq package-user-dir (cask-elpa-path bundle))
    (-each (cask-bundle-sources bundle)
      (lambda (source)
        (epl-add-archive (cask-source-name source)
                         (cask-source-url source))))
    (shut-up
      (condition-case err
          (progn
            (when (plist-get args :refresh)
              (epl-refresh))
            (epl-initialize (not (plist-get args :activate))))
        (error
         (signal 'cask-failed-initialization
                 (list err (shut-up-current-output))))))))

(defun cask--fetcher-dependencies (bundle)
  "Return list of fetcher dependencies for BUNDLE."
  (--select (cask-dependency-fetcher it) (cask--dependencies bundle)))

(defun cask--has-fetcher-dependency-p (bundle)
  "Return true if BUNDLE has any fetcher dependencies."
  (> (length (cask--fetcher-dependencies bundle)) 0))

(defun cask--dependency-to-package-build-recipe (dependency)
  "Turn DEPENDENCY into a package-build recipe object."
  (let ((name (symbol-name (cask-dependency-name dependency)))
        (url (cask-dependency-url dependency))
        (files (cask--dependency-files dependency))
        (fetcher (substring (symbol-name (cask-dependency-fetcher dependency)) 1))
        (commit (cask-dependency-ref dependency))
        (branch (cask-dependency-branch dependency)))
    (cask--create-package-build-recipe
     fetcher name
     :url url :files files :commit commit :branch branch)))

(defun cask--create-package-build-recipe (fetcher name &rest args)
  "Create a package-build `package-recipe' for FETCHER with NAME.
ARGS is the initialization slots."
  (let ((constructor (intern (format "package-%s-recipe" fetcher))))
    (apply constructor name :name name args)))

(defun cask--checkout-and-package-dependency (dependency)
  "Checkout and package DEPENDENCY.

This function returns the path to the package file."
  (--each (list cask-tmp-path cask-tmp-checkout-path cask-tmp-packages-path)
    (unless (f-dir? it) (f-mkdir it)))
  (let ((name (symbol-name (cask-dependency-name dependency)))
        (rcp (cask--dependency-to-package-build-recipe dependency))
        (package-build-working-dir cask-tmp-checkout-path)
        (package-build-archive-dir cask-tmp-packages-path) )
    (cask-print "cloning\e[F\n")
    (let ((version (package-build--checkout rcp)))
      (cask-print "building\e[F\n")
      (package-build--package rcp version)
      (let ((pattern (format "%s-%s.*" name version)))
        (--first (s-match ".*\\.\\(tar\\|el\\)" it)
                 (f-glob pattern cask-tmp-packages-path))))))

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
  `(cask--with-file ,bundle
     (if (or ,(plist-get body :force) (not (equal ,bundle cask-current-bundle)))
         (prog1
             (let ((load-path (-clone load-path)))
               (cask--use-environment ,bundle :refresh ,(plist-get body :refresh))
               ,@body)
           (setq cask-current-bundle (copy-cask-bundle ,bundle)))
       ,@body)))

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

(defun cask--show-package-error (err filename)
  (let ((cause (cl-caddr err)))
    (cond ((string-match-p "ends here" cause)
           (error "Package lacks a footer line in file %s" filename))
          ((string-match-p "cask-cli.el\\'" cause)
           (error "Unbalanced parens in Package-Requires in file %s" filename))
          (t
           (error "%s in file %s" cause filename)))))

(defun cask--eval (bundle forms &optional scope)
  "Populate BUNDLE by evaluating FORMS in SCOPE.

SCOPE may be nil or 'development."
  (-each forms
    (lambda (form)
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
           (let ((package
                  (condition-case err
                      (epl-package-from-file
                       (f-expand filename (cask-bundle-path bundle)))
                    (epl-invalid-package
                     (cask--show-package-error err filename)))))
             (cask--from-epl-package bundle package))))
        (package-descriptor
         (cl-destructuring-bind (_ &optional filename) form
           (let* ((descriptor-filename
                   (or filename (let ((pkg-files (f-glob "*-pkg.el" (cask-bundle-path bundle))))
                                  (if (car pkg-files) (f-filename (car pkg-files))
                                    (error "No -pkg.el file found for package descriptor")))))
                  (package
                   (condition-case err
                      (epl-package-from-descriptor-file
                       (f-expand descriptor-filename (cask-bundle-path bundle)))
                    (epl-invalid-package
                     (cask--show-package-error err descriptor-filename)))))
             (cask--from-epl-package bundle package))))
        (depends-on
         (cl-destructuring-bind (_ name &rest args) form
           (when (stringp (car args))
             (push :version args))
           (setq args (plist-put args :scope scope))
           (apply 'cask-add-dependency (append (list bundle (intern name)) args))))
        (files
         (cl-destructuring-bind (_ &rest patterns) form
           (setf (cask-bundle-patterns bundle) patterns)))
        (development
         (cl-destructuring-bind (_ . body) form
           (cask--eval bundle body 'development)))
        (t
         (error "Unknown directive: %S" form))))))

(defun cask--template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (cask-resource-path "templates"))
         (template-file (f-expand name templates-dir)))
    (f-read-text template-file 'utf-8)))

(defun cask--initialized-p (bundle)
  "Return true if BUNDLE is initialized.

The BUNDLE is initialized when the elpa directory exists."
  (f-dir? (cask-elpa-path bundle)))

(defun cask--epl-package-to-dependency (epl-package)
  "Turn EPL-PACKAGE into a `cask-dependency' object."
  (make-cask-dependency
   :name (epl-package-name epl-package)
   :version (epl-package-version-string epl-package)))

(defun cask--epl-requirement-to-dependency (epl-requirement)
  "Turn EPL-REQUIREMENT into a `cask-dependency' object."
  (make-cask-dependency
   :name (epl-requirement-name epl-requirement)
   :version (epl-requirement-version-string epl-requirement)))

(defun cask--find-available-package (name)
  "Find first available package with NAME."
  (car (epl-find-available-packages name)))

(defun cask--find-installed-package (name)
  "Find installed package of highest version with NAME."
  (car (epl-find-installed-packages name)))

(defun cask--uniq-dependencies (dependencies)
  "Return new list with all duplicates in DEPENDENCIES removed."
  (let ((-compare-fn
         (lambda (dependency-1 dependency-2)
           (eq
            (cask-dependency-name dependency-1)
            (cask-dependency-name dependency-2)))))
    (-uniq dependencies)))

(defun cask--compute-dependencies (dependencies package-function)
  "Return a list of DEPENDENCIES's dependencies, recursively.

PACKAGE-FUNCTION is a function that takes a name as argument and
returns an `epl-package' object."
  (cask--uniq-dependencies
   (-flatten
    (-map
     (lambda (dependency)
       (let ((name (cask-dependency-name dependency)))
         (-when-let (package (funcall package-function name))
           (cask--uniq-dependencies
            (cons dependency
                  (cask--compute-dependencies
                   (-map 'cask--epl-requirement-to-dependency
                         (epl-package-requirements package))
                   package-function))))))
     dependencies))))

(defun cask--runtime-dependencies (bundle &optional deep)
  "Return runtime dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask-bundle-runtime-dependencies bundle)))
    (if deep
        (cask--compute-dependencies dependencies 'cask--find-available-package)
      dependencies)))

(defun cask--development-dependencies (bundle &optional deep)
  "Return development dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask-bundle-development-dependencies bundle)))
    (if deep
        (cask--compute-dependencies dependencies 'cask--find-available-package)
      dependencies)))

(defun cask--dependencies (bundle &optional deep)
  "Return dependencies for BUNDLE, optionally DEEP."
  (cask--uniq-dependencies
   (append (cask--runtime-dependencies bundle deep)
           (cask--development-dependencies bundle deep))))

(defun cask--installed-dependencies (bundle &optional deep)
  "Return installed dependencies for BUNDLE, optionally DEEP."
  (let ((dependencies (cask--dependencies bundle)))
    (if deep
        (cask--compute-dependencies dependencies 'cask--find-installed-package)
      dependencies)))

(defun cask--install-dependency (bundle dependency index)
  "In BUNDLE, install DEPENDENCY.

If dependency does not exist, the error `cask-missing-dependency'
is signaled."
  (let ((name (cask-dependency-name dependency))
        (version (cask-dependency-version dependency)))
    (cask-print
     (format "  - Installing [%2d/%d]" (1+ index) (length (cask--dependencies bundle)))
     " " (green "%s" name) " "
     "(" (yellow "%s" (or version "latest")) ")... ")
    (when (cask-linked-p bundle name)
      (cask-print "linked\n"))
    (when (epl-package-installed-p name)
      (cask-print (bold (black "already present")) "\n"))
    (unless (or (epl-package-installed-p name) (cask-linked-p bundle name))
      (if (cask-dependency-fetcher dependency)
          (shut-up
            (let ((package-path (cask--checkout-and-package-dependency dependency)))
              (epl-install-file package-path)))
        (-if-let (package (cask--find-available-package name))
            (progn
              (cask-print "downloading\e[F\n")
              (shut-up (epl-package-install package)))
          (unless (epl-built-in-p name)
            (cask-print (bold (red "not available")) "\n")
            (signal 'cask-missing-dependency (list dependency)))))
      (cask-print
       (format "\e[K  - Installing [%2d/%d]" (1+ index) (length (cask--dependencies bundle)))
       " " (green "%s" name) " "
       "(" (yellow "%s" (or version "latest")) ")... done\n"))))

(defun cask--delete-dependency (bundle dependency)
  "In BUNDLE, delete DEPENDENCY if it is installed."
  (let ((name (cask-dependency-name dependency)))
    (-when-let (package (epl-find-installed-package name))
      (epl-package-delete package))))

(defun cask--dependency-files (dependency)
  "Return DEPENDENCY files.

Return the files attribute is set.  Otherwise fallback and return
the default files pattern `package-build-default-files-spec'."
  (or (cask-dependency-files dependency) package-build-default-files-spec))

(defun cask--from-epl-package (bundle package)
  "Extend BUNDLE with epl PACKAGE."
  (setf (cask-bundle-name bundle) (epl-package-name package))
  (setf (cask-bundle-version bundle) (epl-package-version-string package))
  (setf (cask-bundle-description bundle) (epl-package-summary package))
  (-each (epl-package-requirements package)
    (lambda (requirement)
      (let ((name (epl-requirement-name requirement))
            (version (epl-requirement-version-string requirement)))
        (cask-add-dependency bundle name :version version)))))


;;;; Public API

(defun cask-setup (project-path)
  "Setup cask for project at PROJECT-PATH.

This function return a `cask-bundle' object."
  (let ((bundle (make-cask-bundle :path (f-canonical project-path))))
    (if (f-file? (cask-file bundle))
        (condition-case err
            (cask--eval bundle (cask--read (cask-file bundle)))
          (end-of-file
           (cask--exit-error bundle err))
          (invalid-read-syntax
           (cask--exit-error bundle err)))
      (-when-let (define-package-file (car (f-glob "*-pkg.el" project-path)))
        (let ((package (epl-package-from-descriptor-file define-package-file)))
          (cask--from-epl-package bundle package))))
    bundle))

(defun cask-initialize (&optional project-path)
  "Initialize packages under PROJECT-PATH or `user-emacs-directory'.

This function return a `cask-bundle' object."
  (let* ((bundle (cask-setup (or project-path user-emacs-directory)))
         (package-load-list
          (-snoc (--map (list (cask-dependency-name it) t)
                        (cask--runtime-dependencies bundle))
                 'all)))
    (when (equal (epl-package-dir) (epl-default-package-dir))
      (cask--use-environment bundle :activate t))
    bundle))

(defun cask-update (bundle)
  "Update BUNDLE dependencies.

Return list of updated packages."
  (cask--with-environment bundle
    :force t
    :refresh t
    (shut-up
      (condition-case err
          (prog1
              (epl-upgrade)
            (--each (cask--fetcher-dependencies bundle)
              (cask--delete-dependency bundle it)
              (cask--install-dependency bundle it it-index)))
        (error
         (signal 'cask-failed-installation
                 (list (car err) err (shut-up-current-output))))))))

(defun cask-outdated (bundle)
  "Return list of `epl-upgrade' objects for outdated BUNDLE dependencies."
  (cask--with-environment bundle
    :force t
    :refresh t
    (epl-find-upgrades)))

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
    (cask-print (green "Loading package information... "))
    (cask--with-environment bundle
      :force t
      :refresh t
      (cask-print (green "done") "\n")
      (cask-print (green "Package operations: %d installs, %d removals\n" (length (cask--dependencies bundle)) 0))
      (-each-indexed (cask--dependencies bundle)
        (lambda (index dependency)
          (condition-case err
              (cask--install-dependency bundle dependency index)
            (cask-missing-dependency
             (push dependency missing-dependencies))
            (error
             (signal 'cask-failed-installation
                     (list dependency err (shut-up-current-output)))))))
      (when missing-dependencies
        (signal 'cask-missing-dependencies (nreverse missing-dependencies))))))

(defun cask-caskify (bundle &optional dev-mode)
  "Create Cask-file for BUNDLE path.

If DEV-MODE is true, the dev template is used, otherwise the
configuration template is used."
  (let ((cask-file (cask-file bundle))
        (init-content
         (cask--template-get (if dev-mode "init-dev.tpl" "init.tpl"))))
    ;; If there's only a single .el file, use that as the package-file.
    (when dev-mode
      (let* ((files (f--files (cask-path bundle) (f-ext? it "el")))
             (package-file (if (equal (length files) 1)
                               (f-filename (-first-item files))
                             "TODO")))
        (setq init-content (format init-content package-file))))
    (if (f-file? cask-file)
        (error "Cask-file already exists")
      (f-write-text init-content 'utf-8 cask-file))))

(defun cask-package-name (bundle)
  "Return BUNDLE name.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-bundle-name bundle))

(defun cask-package-version (bundle)
  "Return BUNDLE version.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-bundle-version bundle))

(defun cask-package-description (bundle)
  "Return BUNDLE description.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-bundle-description bundle))

(defun cask-version ()
  "Return Cask's version."
  (let ((package (epl-package-from-lisp-file
                  (f-expand "cask.el" cask-directory))))
    (epl-package-version-string package)))

(defun cask-load-path (bundle)
  "Return Emacs `load-path' (including BUNDLE dependencies)."
  (cask--with-environment bundle
    (append
     (-map 'f-expand (-uniq (-map 'f-parent (cask-files bundle))))
     (-map
      (lambda (dependency)
        (cask-dependency-path bundle (cask-dependency-name dependency)))
      (cask--installed-dependencies bundle 'deep))
     load-path)))

(defun cask-exec-path (bundle)
  "Return Emacs `exec-path' (including BUNDLE dependencies)."
  (cask--with-environment bundle
    (append
     (-map 'expand-file-name (-uniq (-map 'f-parent (-filter 'f-executable-p (cask-files bundle)))))
     (-select
      'f-dir?
      (-map
       (lambda (dependency)
         (let ((path (cask-dependency-path bundle (cask-dependency-name dependency))))
           (f-expand "bin" path)))
       (cask--installed-dependencies bundle 'deep)))
     exec-path)))

(defun cask-elpa-path (bundle)
  "Return full path to BUNDLE elpa directory."
  (f-expand (format ".cask/%s/elpa"
                    cask-bootstrap-emacs-version)
            (cask-bundle-path bundle)))

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
  (not (null (cask-find-dependency bundle name))))

(defun cask-find-dependency (bundle name)
  "Find dependency in BUNDLE with NAME."
  (-first
   (lambda (dependency)
     (eq name (cask-dependency-name dependency)))
   (cask--dependencies bundle)))

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
    (f-expand (format "%s-pkg.el" (cask-bundle-name bundle)) (cask-bundle-path bundle))))

(defun cask-dependency-path (bundle name)
  "Return path to BUNDLE dependency with NAME.

If no such dependency exist, return nil."
  (car (f-glob (format "%s-*" name) (cask-elpa-path bundle))))

(defun cask-path (bundle)
  "Return BUNDLE root path."
  (cask-bundle-path bundle))

(defun cask-file (bundle)
  "Return path to BUNDLE Cask-file."
  (f-expand cask-filename (cask-path bundle)))

(defun cask-files (bundle)
  "Return BUNDLE files list.

This is done by expanding the patterns in the BUNDLE path.  Files
in the list are relative to the path."
  (cask--with-file bundle
    (let* ((path (cask-bundle-path bundle))
           (file-list (cask-bundle-patterns bundle))
           ;; stolen from `package-build--config-file-list'
           (patterns (cond ((null file-list)
                            package-build-default-files-spec)
                           ((eq :defaults (car file-list))
                            (append package-build-default-files-spec (cdr file-list)))
                           (t
                            file-list))))
      (-map 'car (ignore-errors (package-build-expand-file-specs path patterns))))))

(defun cask-add-dependency (bundle name &rest args)
  "Add dependency to BUNDLE.

NAME is the name of the dependency.

ARGS is a plist with these optional arguments:

 `:version' Depend on at least this version for this dependency.

 `:scope' Add dependency to a certain scope.  Allowed values are
 'development and 'runtime.

 `:files' Only include files matching this pattern.

 `:ref' Fetcher ref to checkout.

 `:branch' Fetcher branch to checkout.

ARGS can also include any of the items in `cask-fetchers'.  The
plist key is one of the items in the list and the value is the
url to the fetcher source."
  (let ((dependency (make-cask-dependency :name name)))
    (-when-let (version (plist-get args :version))
      (setf (cask-dependency-version dependency) version))
    (-when-let (files (plist-get args :files))
      (setf (cask-dependency-files dependency) files))
    (-when-let (fetcher (--first (-contains? cask-fetchers it) args))
      (setf (cask-dependency-fetcher dependency) fetcher)
      (let ((url (plist-get args fetcher)))
        (setf (cask-dependency-url dependency) url))
      (-when-let (ref (plist-get args :ref))
        (setf (cask-dependency-ref dependency) ref))
      (-when-let (branch (plist-get args :branch))
        (setf (cask-dependency-branch dependency) branch)))
    (if (eq (plist-get args :scope) 'development)
        (push dependency (cask-bundle-development-dependencies bundle))
      (push dependency (cask-bundle-runtime-dependencies bundle)))))

(defun cask-add-source (bundle name-or-alias &optional url)
  "Add source to BUNDLE.

NAME-OR-ALIAS is either a string with the name of the source or a
symbol, which refers to some of the keys in
`cask-source-mapping'.

Second argument URL is only required unless alias.  If no alias,
URL is the url to the mirror."
  (unless url
    (let ((mapping (assq name-or-alias cask-source-mapping)))
      (unless mapping
        (error "Unknown package archive: %s" name-or-alias))
      (setq name-or-alias (symbol-name (car mapping)))
      (setq url (cdr mapping))))
  (push (make-cask-source :name name-or-alias :url url) (cask-bundle-sources bundle)))

(defun cask-remove-source (bundle name)
  "Remove source from BUNDLE with NAME."
  (let ((sources (--reject (string= name (cask-source-name it))
                           (cask-bundle-sources bundle))))
    (setf (cask-bundle-sources bundle) sources)))

(defun cask-build (bundle)
  "Build BUNDLE Elisp files."
  (cask--with-file bundle
    (require 'bytecomp)
    (let ((load-path (cons (cask-path bundle) (cask-load-path bundle))))
      (-each (cask-files bundle)
        (lambda (path)
          (when (and (f-file? path) (f-ext? path "el"))
            (if (fboundp 'byte-recompile-file)
                (byte-recompile-file path 'force 0)
              (byte-compile-file path nil))))))))

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
the path to the directory to link to.  SOURCE must have either a
NAME-pkg.el or Cask file for the linking to be possible."
  (cask--with-file bundle
    (setq source (f-expand source))
    (unless (cask-has-dependency bundle name)
      (error "Cannot link package %s, is not a dependency" name))
    (unless (f-dir? source)
      (error "Cannot create link %s to non existing path: %s" name source))
    (let ((link-bundle (cask-setup source)))
      (unless (f-file? (f-expand (format "%s-pkg.el" name) source))
        (if (f-file? (f-expand cask-filename source))
            (f-write-text (cask-define-package-string link-bundle) 'utf-8
                          (cask-define-package-file link-bundle))
          (error "Link source %s does not have a Cask or %s-pkg.el file"
                 source name)))
      (when (cask--initialized-p bundle)
        (let ((link-path
               (f-expand
                (format "%s-%s"
                        (cask-package-name link-bundle)
                        (cask-package-version link-bundle))
                (cask-elpa-path bundle))))
          (when (f-exists? link-path)
            (f-delete link-path 'force))
          (-when-let (dependency-path (cask-dependency-path bundle name))
            (f-delete dependency-path 'force))
          (f-symlink source link-path))))))

(defun cask-link-delete (bundle name)
  "Delete BUNDLE link with NAME."
  (cask--with-file bundle
    (unless (cask-has-dependency bundle name)
      (error "Cannot link package %s, is not a dependency" name))
    (let ((link (cask-dependency-path bundle name)))
      (if (and link (f-symlink? link))
          (f-delete link)
        (error "Package %s not linked" name)))))

(defun cask-linked-p (bundle name)
  "Return true if BUNDLE has link with NAME."
  (-when-let (path (cask-dependency-path bundle name))
    (f-symlink? path)))

(defun cask-package (bundle &optional target-dir)
  "Build an ELPA package of BUNDLE.

Put package in TARGET-DIR if specified.  If not specified, put in
a directory specified by `cask-dist-path' in the BUNDLE path."
  (cask--with-package bundle
    (let ((name (symbol-name (cask-bundle-name bundle)))
          (version (cask-bundle-version bundle))
          (patterns (or (cask-bundle-patterns bundle)
                        package-build-default-files-spec))
          (path (cask-bundle-path bundle)))
      (if target-dir
          (setq target-dir (f-expand target-dir))
        (setq target-dir (f-expand cask-dist-path path)))
      (unless (f-dir? target-dir)
        (f-mkdir target-dir))
      (let ((rcp (package-directory-recipe name
                  :name name
                  :files patterns
                  :dir path))
            (package-build-working-dir path)
            (package-build-archive-dir target-dir))
        (package-build--package rcp version)))))

(provide 'cask)

;;; cask.el ends here
