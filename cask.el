;;; cask.el --- Cask: Project management for package development  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2012-2014 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.8.9pre
;; Keywords: speed, convenience
;; URL: http://github.com/cask/cask
;; Package-Requires: ((emacs "24.5") (s "1.8.0") (f "0.16.0") (epl "0.5") (shut-up "0.1.0") (cl-lib "0.3") (package-build "0") (ansi "0.4.1"))

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
(define-error 'cask-empty-archive-contents "Empty archive contents" 'cask-error)

(defmacro cask--shut-up-unless-debug (&rest body)
  "The shut-up module is singularly designed to defeat *every*
 attempt at making your elisp package tractable."
  (declare (indent defun))
  `(if debug-on-error
       (cl-flet ((shut-up-current-output () (ignore)))
	 ,@body)
     (shut-up ,@body)))

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

(cl-defmethod package-recipe--working-tree ((rcp package-directory-recipe))
  (oref rcp dir))

(cl-defmethod package-build--get-commit ((_rcp package-directory-recipe)))

(defvar cask-source-mapping
  `((gnu          . ,(concat (if (< emacs-major-version 27) "http" "https")
                             "://elpa.gnu.org/packages/"))
    (celpa        . "https://celpa.conao3.com/packages/")
    (shmelpa      . "https://shmelpa.commandlinesystems.com/packages/")
    (melpa        . "https://melpa.org/packages/")
    (melpa-stable . "https://stable.melpa.org/packages/")
    (marmalade    . "https://marmalade-repo.org/packages/")
    (nongnu       . "https://elpa.nongnu.org/nongnu/")
    (org          . "https://orgmode.org/elpa/"))
  "Mapping of source name and url.")

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

(defconst cask-dist-path "dist"
  "Name of default target directory for building packages.")

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

(cl-defmacro cask-print (&rest body &key stderr &allow-other-keys)
  "Print messages to `standard-output'.

The BODY of this macro is automatically wrapped with
`with-ansi' for easier colored output."
  (delq :stderr body)
  `(when (or (not (boundp 'cask-cli--silent))
             (not cask-cli--silent))
     (princ (with-ansi ,@body) ,(when stderr '(function external-debugging-output)))))

(defun cask-warn (message &rest args)
  "Display MESSAGE with ARGS.  see `warn'."
  (display-warning 'cask (apply #'format message args)))

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

(defun cask--use-environment (bundle refresh no-activate)
  "If called outside `cask--with-environment', alter package context."
  (setq package-archives nil)
  (setq package-user-dir (cask-elpa-path bundle))
  (dolist (source (cask-bundle-sources bundle))
    (epl-add-archive (cask-source-name source) (cask-source-url source)))
  (cask--shut-up-unless-debug
    (condition-case err
        (progn
          (epl-initialize no-activate)
          (when refresh (epl-refresh)))
      (error (signal 'cask-failed-initialization
                     (list err (shut-up-current-output)))))))

(cl-defmacro cask--with-environment (bundle
                                     &rest body
                                     &key activate refresh
                                     &allow-other-keys
                                     &aux (no-activate (not activate)))
  "Hygienic wrapper for `cask--use-environment'.
Evaluate BODY in the package context of BUNDLE.  Then restore package context."
  `(cask--with-file ,bundle
     (let* (package-alist
            package-activated-list
            package-archives
            package-archive-contents
            (load-path load-path)
            (package-directory-list
             (eval (car (get 'package-directory-list 'standard-value))))
            (package-load-list
             (eval (car (get 'package-load-list 'standard-value))))
            (package-user-dir (cask-elpa-path ,bundle))
	    (,@(if (special-variable-p 'package-gnupghome-dir)
		   (cons 'package-gnupghome-dir (list (expand-file-name "gnupg" package-user-dir)))
		 (list '_package-gnupghome-dir))))
       (cask--use-environment ,bundle ,refresh ,no-activate)
       ;; following will evaluate keys e.g., `:activate t` as separate s-exprs
       ;; which ought to be no-ops
       ,@body)))

(defun cask--dependencies-and-missing (bundle)
  "Workhorse for `cask--dependencies'."
  (let* (missing
         (errback (lambda (dep)
                    (push dep missing)))
         (runtime (cask--runtime-dependencies bundle errback))
         (develop (cask--development-dependencies bundle errback)))
    (list runtime develop (cask--uniq-dependencies missing))))

(defun cask--dependencies (bundle &optional _deep)
  "Return transitive closure of dependencies for BUNDLE.
The legacy argument _DEEP is assumed true."
  (cl-destructuring-bind (runtime develop &rest args)
      (cask--dependencies-and-missing bundle)
    (cask--uniq-dependencies (append runtime develop))))

(defun cask--fetcher-dependencies (bundle)
  "Return list of fetcher dependencies for BUNDLE."
  (cl-remove-if-not #'cask-dependency-fetcher (cask--dependencies bundle)))

(defun cask--has-fetcher-dependency-p (bundle)
  "Return t if BUNDLE has any fetcher dependencies."
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
  (dolist (elm (list cask-tmp-path cask-tmp-checkout-path cask-tmp-packages-path))
    (unless (f-dir? elm) (f-mkdir elm)))
  (let ((name (symbol-name (cask-dependency-name dependency)))
        (rcp (cask--dependency-to-package-build-recipe dependency))
        (package-build-working-dir cask-tmp-checkout-path)
        (package-build-archive-dir cask-tmp-packages-path) )
    (cask-print "cloning\e[F\n")
    (let ((version (package-build--checkout rcp)))
      (cask-print "building\e[F\n")
      (package-build--package rcp version)
      (let ((pattern (format "%s-%s.*" name version)))
        (cl-find-if
         (lambda (elm) (s-match ".*\\.\\(tar\\|el\\)" elm))
         (f-glob pattern cask-tmp-packages-path))))))

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
  "Show package error.
ERR is error object, FILENAME is the name of related file."
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
  (dolist (form forms)
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
         (apply #'cask-add-dependency (append (list bundle (intern name)) args))))
      (files
       (cl-destructuring-bind (_ &rest patterns) form
         (setf (cask-bundle-patterns bundle) patterns)))
      (development
       (cl-destructuring-bind (_ . body) form
         (cask--eval bundle body 'development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun cask--initialized-p (bundle)
  "Return t if BUNDLE is initialized.

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

(defun cask--find-available-package-jit (bundle name version)
  "Ensure `cask--find-available-package' has `package-archive-contents'."
  (when (and (not package-archive-contents)
             (cask-bundle-sources bundle))
    (epl-refresh)
    (unless package-archive-contents
      (signal 'cask-empty-archive-contents
              (list (mapcar #'cask-source-url
                            (cask-bundle-sources bundle))))))
  (when (or version (not (epl-built-in-p name)))
    (cask--find-available-package name)))

(defun cask--find-available-package (name)
  "Find first available package with NAME."
  (car (epl-find-available-packages name)))

(defun cask--find-installed-package (name)
  "Find installed package of highest version with NAME."
  (car (epl-find-installed-packages name)))

(defun cask--uniq-dependencies (dependencies)
  "Return new list with all duplicates in DEPENDENCIES removed."
  (cl-delete-duplicates
   dependencies
   :test (lambda (a b)
           (eq (cask-dependency-name a) (cask-dependency-name b)))))

(defun cask--compute-dependencies (dependencies package-function errback)
  "Topologically sort full dependency graph.

PACKAGE-FUNCTION is a function that takes a name and version as argument and
returns an `epl-package' object."
  (cl-loop with seen = (mapcar #'cask-dependency-name dependencies)
           with queue = dependencies
           until (null queue)
           for dep = (pop queue)
           for name = (cask-dependency-name dep)
	   for version = (cask-dependency-version dep)
           for fetcher = (cask-dependency-fetcher dep)
           for package = (funcall package-function name version)
           if fetcher
             collect dep into result
           else
             if package
               collect dep into result
               and do (dolist (req (epl-package-requirements package))
                        (let ((child-dep
                               (cask--epl-requirement-to-dependency req)))
                          (unless (memq (cask-dependency-name child-dep) seen)
                            (setq queue (append queue (list child-dep)))
                            (push (cask-dependency-name child-dep) seen))))
             else
               if (eq name 'emacs)
                 collect dep into result
               else
                 do (unless (epl-built-in-p name) (funcall errback dep))
               end
             end
           end
           finally return result))

(defun cask--runtime-dependencies (bundle &optional errback)
  "Return runtime dependencies for BUNDLE.
The legacy argument _DEEP is assumed true."
  (cask--compute-dependencies
   (cask-bundle-runtime-dependencies bundle)
   (apply-partially #'cask--find-available-package-jit bundle)
   (or errback #'ignore)))

(defun cask--development-dependencies (bundle &optional errback)
  "Return development dependencies for BUNDLE.
The legacy argument _DEEP is assumed true."
  (cask--compute-dependencies
   (cask-bundle-development-dependencies bundle)
   (apply-partially #'cask--find-available-package-jit bundle)
   (or errback #'ignore)))

(defun cask--remove-vendored (dependencies)
  "Upper bound for package-build for emacs-24 is vendored."
  (cl-remove-if
   (lambda (dep) (and (version< emacs-version "25.1")
                      (eq (cask-dependency-name dep) 'package-build)))
   dependencies))

(defun cask--installed-dependencies (bundle &optional _deep)
  "Return installed dependencies for BUNDLE.
The legacy argument _DEEP is assumed true."
  (cl-remove-if-not (lambda (dep) (cask--find-installed-package
                                   (cask-dependency-name dep)))
                    (cask--dependencies bundle)))

(defun cask--legacy-dependency-installed-p (bundle dependency)
  "Version-ignorant predicate.  Useful only for emacs24."
  (cl-assert (< emacs-major-version 25))
  (let ((name (cask-dependency-name dependency)))
    (or (epl-package-installed-p name) (cask-linked-p bundle name))))

(defun cask--dependency-installed-p (bundle dependency)
  (let* ((name (cask-dependency-name dependency))
         (version (cask-dependency-version dependency))
	 (version* (if (listp version) version (version-to-list version))))
    (if (fboundp 'package-desc-create)
	(epl-package-installed-p
	 (epl-package-create
	  :name name
	  :description (package-desc-create :name name :version version*)))
      (cask--legacy-dependency-installed-p bundle dependency))))

(defun cask--install-dependency (bundle dependency index total)
  "In BUNDLE, install DEPENDENCY.

If dependency does not exist, the error `cask-missing-dependency'
is signaled.
INDEX is the current install index of TOTAL indices."
  (let* ((name (cask-dependency-name dependency))
	 (version (cask-dependency-version dependency))
	 (version* (if (listp version) version (version-to-list version))))
    (cask-print
     (format "  - Installing [%2d/%d]" (1+ index) total)
     " " (green "%s" name) " "
     "(" (yellow "%s" (or version "latest")) ")... ")
    (when (cask-linked-p bundle name)
      (cask-print "linked\n"))
    (if (cask--dependency-installed-p bundle dependency)
	(cask-print (bold (black "already present")) "\n")
      (if (cask-dependency-fetcher dependency)
          (cask--shut-up-unless-debug
            (let ((package-path (cask--checkout-and-package-dependency dependency)))
              (epl-refresh)
              (epl-install-file package-path)))
	(let ((package (cask--find-available-package name)))
          (cond ((and version*
		      (epl-package-p package)
		      (epl-package-version package)
		      (version-list-< (epl-package-version package) version*))
		 (cask-print (bold (red "not available")) "\n")
		 (signal 'cask-missing-dependency (list dependency)))
		((and (null package)
		      (epl-built-in-p name))
		 (cask-print (bold (red "not available")) "\n")
		 (signal 'cask-missing-dependency (list dependency)))
		(t (cask-print "downloading\e[F\n")
		   (cask--shut-up-unless-debug
		     (condition-case-unless-debug err
			 ;; `epl-package-install' does not correctly
			 ;; catch errors, so we get "Wrong type argument"
			 ;; on failed `package-compute-transaction'
			 (epl-package-install package)
		       (error
			(signal 'cask-missing-dependency
				(list (error-message-string err)
				      (shut-up-current-output))))))))))
      (cask-print
       (format "\e[K  - Installing [%2d/%d]" (1+ index) total)
       " " (green "%s" name) " "
       "(" (yellow "%s" (or version "latest")) ")... done\n"))))

(defun cask--delete-dependency (_bundle dependency)
  "In BUNDLE, delete DEPENDENCY if it is installed."
  (let* ((name (cask-dependency-name dependency))
         (package (epl-find-installed-packages name)))
    (when package
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
  (dolist (requirement (epl-package-requirements package))
    (let ((name (epl-requirement-name requirement))
          (version (epl-requirement-version-string requirement)))
      (cask-add-dependency bundle name :version version))))


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
      (let ((define-package-file (car (f-glob "*-pkg.el" project-path))))
        (when define-package-file
          (let ((package (epl-package-from-descriptor-file define-package-file)))
            (cask--from-epl-package bundle package)))))
    bundle))

(defun cask-initialize (&optional project-path)
  "In May 2022, this function was repurposed for `cask emacs` invocations.
As such, it may not be backwards-compatible with earlier contexts."
  (let* (package--initialized
	 (bundle (cask-setup (or project-path user-emacs-directory)))
         (package-load-list
          `(,@(mapcar
               (lambda (elm) (list (cask-dependency-name elm) t))
               (cask-runtime-dependencies bundle))
            all)))
    (cask--use-environment bundle nil nil)
    (setq user-emacs-directory (expand-file-name ".cask" (cask-bundle-path bundle)))
    bundle))

(defun cask-update (bundle)
  "Update BUNDLE dependencies.

Return list of updated packages."
  (cask--with-environment bundle
    :refresh t
    (cask--shut-up-unless-debug
      (condition-case-unless-debug err
          (prog1 (epl-upgrade)
            (let* ((deps (cask--fetcher-dependencies bundle))
                   (total (length deps)))
              (dotimes (inx total)
                (cask--delete-dependency bundle (nth inx deps))
                (cask--install-dependency bundle (nth inx deps) inx total))))
        (error (signal 'cask-failed-installation
                       (list (error-message-string err) (shut-up-current-output))))))))

(defun cask-outdated (bundle)
  "Return list of `epl-upgrade' objects for outdated BUNDLE dependencies."
  (cask--with-environment bundle
    :refresh t
    (epl-find-upgrades)))

(defun cask--build-install-dependencies (bundle)
  "Maybe incur cost of \"cask install\" before attempting to byte-compile."
  (unless (cl-every
	   (apply-partially #'cask--dependency-installed-p bundle)
           (cask-runtime-dependencies bundle))
    (cask-install bundle)))

(defun cask-list (bundle)
  "List BUNDLE dependencies."
  (cask--with-environment bundle
    (cl-destructuring-bind (runtime develop &rest args)
        (cask--dependencies-and-missing bundle)
      (princ "### Dependencies ###\n\n")
      (princ (format "Runtime [%s]:\n" (length runtime)))
      (mapc 'cask-cli--print-dependency runtime)
      (if (> (length runtime) 0)
          (princ "\n"))
      (princ (format "Development [%s]:\n" (length develop)))
      (mapc 'cask-cli--print-dependency develop))))

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
  (cask-print :stderr (green "Loading package information... "))
  (cask--with-environment bundle
    :refresh t
    (cl-destructuring-bind (runtime
                            develop
                            missing-dependencies
                            &aux
                            (dependencies (cask--remove-vendored
                                           (cask--uniq-dependencies
                                            (append runtime develop))))
                            (total (length dependencies)))
        (cask--dependencies-and-missing bundle)
      (cask-print :stderr (green "done") "\n")
      (cask-print (green "Package operations: %d installs, %d removals\n" total 0))
      (dotimes (inx total)
        (cask--install-dependency bundle (nth inx dependencies) inx total))
      (when missing-dependencies
        (signal 'cask-missing-dependencies missing-dependencies)))))

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
     (mapcar #'f-expand (delete-dups (mapcar #'f-parent (cask-files bundle))))
     (mapcar
      (lambda (dependency)
        (cask-dependency-path bundle (cask-dependency-name dependency)))
      (cask--installed-dependencies bundle))
     load-path)))

(defun cask-exec-path (bundle)
  "Return Emacs `exec-path' (including BUNDLE dependencies)."
  (cask--with-environment bundle
    (append
     (mapcar #'expand-file-name (delete-dups (mapcar #'f-parent (cl-remove-if-not #'f-executable-p (cask-files bundle)))))
     (cl-remove-if-not
      #'f-dir?
      (mapcar
       (lambda (dependency)
         (let ((path (cask-dependency-path bundle (cask-dependency-name dependency))))
           (f-expand "bin" path)))
       (cask--installed-dependencies bundle)))
     exec-path)))

(defun cask-elpa-path (bundle)
  "Return full path to BUNDLE elpa directory."
  (f-expand
   (format ".cask/%s.%s/elpa" emacs-major-version emacs-minor-version)
   (cask-bundle-path bundle)))

(defun cask-runtime-dependencies (bundle &optional _deep)
  "Return BUNDLE's runtime dependencies.
The legacy argument _DEEP is assumed true.
Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--runtime-dependencies bundle)))

(defun cask-development-dependencies (bundle &optional _deep)
  "Return BUNDLE's development dependencies.
The legacy argument _DEEP is assumed true.
Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--development-dependencies bundle)))

(defun cask-dependencies (bundle &optional _deep)
  "Return BUNDLE's runtime and development dependencies.
The legacy argument _DEEP is assumed true.
Return value is a list of `cask-dependency' objects."
  (cask--with-environment bundle
    (cask--dependencies bundle)))

(defun cask-installed-dependencies (bundle &optional _deep)
  "Return list of BUNDLE's installed dependencies.
The legacy argument _DEEP is assumed true."
  (cask--with-environment bundle
    (cask--installed-dependencies bundle)))

(defun cask-has-dependency (bundle name)
  "Return t if BUNDLE contain link with NAME, false otherwise."
  (cask-find-dependency bundle name))

(defun cask-find-dependency (bundle name)
  "Find dependency in BUNDLE with NAME."
  (cl-find-if
   (lambda (dependency)
     (eq name (cask-dependency-name dependency)))
   (cask-dependencies bundle)))

(defun cask-define-package-string (bundle)
  "Return `define-package' string for BUNDLE."
  (cask--with-package bundle
    (let ((name (symbol-name (cask-bundle-name bundle)))
          (version (cask-bundle-version bundle))
          (description (cask-bundle-description bundle))
          (dependencies
           (mapcar
            (lambda (dependency)
              (list (cask-dependency-name dependency)
                    (cask-dependency-version dependency)))
            (cask-runtime-dependencies bundle))))
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
      (mapcar #'car (ignore-errors (package-build-expand-file-specs path patterns))))))

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
    (when (plist-get args :version)
      (setf (cask-dependency-version dependency) (plist-get args :version)))
    (when (plist-get args :files)
      (setf (cask-dependency-files dependency) (plist-get args :files)))
    (let ((fetcher (cl-find-if (lambda (elm) (memq elm cask-fetchers)) args)))
      (when fetcher
        (setf (cask-dependency-fetcher dependency) fetcher)
        (let ((url (plist-get args fetcher)))
          (setf (cask-dependency-url dependency) url))
        (when (plist-get args :ref)
          (setf (cask-dependency-ref dependency) (plist-get args :ref)))
        (when (plist-get args :branch)
          (setf (cask-dependency-branch dependency) (plist-get args :branch)))))
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
  (let ((sources (cl-remove-if
                  (lambda (elm) (string= name (cask-source-name elm)))
                  (cask-bundle-sources bundle))))
    (setf (cask-bundle-sources bundle) sources)))

(defun cask-build (bundle)
  "Build BUNDLE Elisp files."
  (cask--with-environment bundle
    (require 'bytecomp)
    (cask--build-install-dependencies bundle)
    (let ((load-path (cons (cask-path bundle) (cask-load-path bundle))))
      (dolist (path (cask-files bundle))
        (when (and (f-file? path) (f-ext? path "el"))
          (if (fboundp 'byte-recompile-file)
              (byte-recompile-file path 'force 0)
            (byte-compile-file path)))))))

(defun cask-clean-elc (bundle)
  "Remove BUNDLE Elisp byte compiled files."
  (cask--with-file bundle
    (dolist (path (cask-files bundle))
      (when (and (f-file? path) (f-ext? path "el"))
        (when (f-file? (concat path "c"))
          (f-delete (concat path "c")))))))

(defun cask-links (bundle)
  "Return a list of all links for BUNDLE.

The list is a list of alist's where the key is the name of the
link, as a string and the value is the absolute path to the link."
  (cask--with-file bundle
    (when (cask--initialized-p bundle)
      (mapcar
       (lambda (file)
         (list (f-filename file) (f-canonical file)))
       (f-entries (cask-elpa-path bundle) 'f-symlink?)))))

(defun cask-link (bundle name source)
  "Add BUNDLE link with NAME to SOURCE.

NAME is the name of the package to link as a string.  SOURCE is
the path to the directory to link to.  SOURCE must have either a
NAME-pkg.el or Cask file for the linking to be possible."
  (cask--with-environment bundle
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
          (when (cask-dependency-path bundle name)
            (f-delete (cask-dependency-path bundle name) 'force))
          (f-symlink source link-path))))))

(defun cask-link-delete (bundle name)
  "Delete BUNDLE link with NAME."
  (cask--with-environment bundle
    (unless (cask-has-dependency bundle name)
      (error "Cannot link package %s, is not a dependency" name))
    (let ((link (cask-dependency-path bundle name)))
      (if (and link (f-symlink? link))
          (f-delete link)
        (error "Package %s not linked" name)))))

(defun cask-linked-p (bundle name)
  "Return t if BUNDLE has link with NAME."
  (cask--with-environment bundle
    (when (cask-dependency-path bundle name)
      (f-symlink? (cask-dependency-path bundle name)))))

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
