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
(define-error 'cask-not-a-package "Missing `package` or `package-file` directive" 'cask-error)
(define-error 'cask-no-cask-file "Cask file does not exist" 'cask-error)
(define-error 'cask-no-such-link "No such link" 'cask-error)

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

`files' List of files specified from expanding arguments to
files-directive. The list contains absolute paths.

`sources' List of `cask-source' objects."
  name version description runtime-dependencies
  development-dependencies path files sources)

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
    (org         . "http://orgmode.org/elpa/")
    (cask-test   . "http://127.0.0.1:9191/packages/"))
  "Mapping of source name and url.")

(defvar cask-links-file
  (f-expand (format ".cask/%s/links" emacs-version) user-emacs-directory)
  "Path to file where links are stored.")

(defconst cask-filename "Cask"
  "Name of the `Cask` file.")

(defconst cask-link-suffix "-dev"
  "Append link name with this, to a simulate version.")


;;;; Internal functions

(defun cask-expand-files (path &optional patterns)
  "Expand absolute files relative to PATH list of PATTERNS."
  (--map (f-expand it path)
         (ignore-errors
           (pb/expand-source-file-list path `(:files ,patterns)))))

(defun cask-find-unbalanced-parenthesis (bundle)
  (with-temp-buffer
    (insert (f-read-text (cask-file bundle) 'utf-8))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (check-parens)
          nil)
      (error (cask-current-source-position)))))

(defun cask-exit-error (bundle err)
  (let ((type (car err))
        (data (cdr err))
        pos msg)
    (if (eq type 'end-of-file)
        ;; In case of premature end of file, try hard to find the real
        ;; position, by scanning for unbalanced parenthesis
        (setq pos (or (cask-find-unbalanced-parenthesis bundle) (cadr err))
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

(defun cask-packages (bundle)
  "Return list of `epl-package' objects for BUNDLE dependencies."
  (-map
   (lambda (dependency)
     (epl-find-installed-package (cask-dependency-name dependency)))
   (cask-dependencies bundle)))

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

(defun epl-change-package-dir (directory)
  "Change the directory of packages to DIRECTORY."
  (setq package-user-dir directory))

(defmacro cask-use-environment (bundle &rest body)
  "Use environment specified by BUNDLE and yield BODY."
  (declare (indent 1) (debug t))
  `(let ((sources (cask-bundle-sources bundle)))
     (setq package-archives nil)        ; TODO: Should call
                                        ; `epl-reset' here, but it
                                        ; does not work as expected.
     (epl-change-package-dir (cask-elpa-dir bundle))
     (-each sources
            (lambda (source)
              (epl-add-archive (cask-source-name source)
                               (cask-source-url source))))
     (epl-refresh)
     (epl-initialize)
     ,@body))

(defmacro cask-with-file (bundle &rest body)
  "If BUNDLE path has a Cask-file, yield BODY.

If BUNDLE is not a package, the error `cask-no-cask-file' is signaled."
  (declare (indent 1) (debug t))
  `(if (f-file? (cask-file bundle))
       (progn ,@body)
     (signal 'cask-no-cask-file (list (cask-file bundle)))))

(defmacro cask-with-package (bundle &rest body)
  "If BUNDLE is a package, yield BODY.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (declare (indent 1) (debug t))
  `(cask-with-file bundle
     (if (and
          (cask-bundle-name bundle)
          (cask-bundle-version bundle)
          (cask-bundle-description bundle))
         (progn ,@body)
       (signal 'cask-not-a-package nil))))

(defun cask-eval (bundle forms &optional scope)
  "Evaluate cask FORMS in SCOPE.

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
       (cl-destructuring-bind (_ &rest args) form
         (let ((files (cask-expand-files (cask-bundle-path bundle) args)))
           (setf (cask-bundle-files bundle) files))))
      (development
       (cl-destructuring-bind (_ . body) form
         (cask-eval bundle body :development)))
      (t
       (error "Unknown directive: %S" form)))))

(defun cask-template-get (name)
  "Return content of template with NAME."
  (let* ((templates-dir (cask-resource-path "templates"))
         (template-file (f-expand name templates-dir)))
    (f-read-text template-file 'utf-8)))

(defun cask-write-links (links)
  "Write LINKS to `cask-links-file'."
  (f-write-text (pp-to-string links) 'utf-8 cask-links-file))


;;;; Public API

(defun cask-setup (project-path)
  "Setup cask for project at PROJECT-PATH."
  (let ((bundle (make-cask-bundle :path (f-long project-path))))
    (when (f-file? (cask-file bundle))
      (condition-case err
          (cask-eval bundle (cask-read (cask-file bundle)))
        (end-of-file
         (cask-exit-error bundle err))
        (invalid-read-syntax
         (cask-exit-error bundle err))))
    bundle))

(defun cask-elpa-dir (bundle)
  "Return full path to BUNDLE elpa directory."
  (f-expand (format ".cask/%s/elpa" emacs-version) (cask-bundle-path bundle)))

(defun cask-initialize (&optional project-path)
  "Initialize packages under PROJECT-PATH (defaults to `user-emacs-directory').
Setup `package-user-dir' appropriately and then call `package-initialize'."
  (let ((bundle (cask-setup (or project-path user-emacs-directory))))
    (epl-initialize)
    bundle))

(defun cask-update (bundle)
  "Update BUNDLE dependencies.

Return list of updated packages."
  (cask-use-environment bundle
    (epl-upgrade (cask-packages bundle))))

(defun cask-outdated (bundle)
  "Return list of `epl-upgrade' objects for outdated BUNDLE dependencies."
  (cask-use-environment bundle
    (epl-find-upgrades (cask-packages bundle))))

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
    (-when-let (dependencies (cask-dependencies bundle))
      (cask-use-environment bundle
        (cl-dolist (dependency dependencies)
          (let ((name (cask-dependency-name dependency)))
            (unless (epl-package-installed-p name)
              (let ((package (car (epl-find-available-packages name))))
                (if package
                    (condition-case err
                        (epl-package-install package)
                      (error (signal 'cask-failed-installation (cons dependency err))))
                  (push dependency missing-dependencies)))))))
      (when missing-dependencies
        (signal 'cask-missing-dependencies (nreverse missing-dependencies))))))

(defun cask-caskify (path &optional dev-mode)
  "Create Cask-file in PATH.

If DEV-MODE is true, the dev template is used, otherwise the
configuration template is used."
  (let ((init-content
         (cask-template-get (if dev-mode "init-dev.tpl" "init.tpl")))
        (cask-file (f-expand cask-filename path)))
    (if (f-file? cask-file)
        (error "Cask-file already exists")
      (f-write-text init-content 'utf-8 cask-file))))

(defun cask-package-name (bundle)
  "Return BUNDLE name.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-with-package bundle (cask-bundle-name bundle)))

(defun cask-package-version (bundle)
  "Return BUNDLE version.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-with-package bundle (cask-bundle-version bundle)))

(defun cask-package-description (bundle)
  "Return BUNDLE description.

If BUNDLE is not a package, the error `cask-not-a-package' is signaled."
  (cask-with-package bundle (cask-bundle-description bundle)))

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
  (cask-with-file bundle (cask-bundle-runtime-dependencies bundle)))

(defun cask-development-dependencies (bundle)
  "Return BUNDLE's development dependencies.

Return value is a list of `cask-dependency' objects."
  (cask-with-file bundle (cask-bundle-development-dependencies bundle)))

(defun cask-dependencies (bundle)
  "Return BUNDLE's runtime and development dependencies.

Return value is a list of `cask-dependency' objects."
  (append (cask-runtime-dependencies bundle)
          (cask-development-dependencies bundle)))

(defun cask-define-package-string (bundle)
  "Return `define-package' string for BUNDLE."
  (cask-with-package bundle
    (let ((name (symbol-name (cask-bundle-name bundle)))
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
  (cask-with-package bundle
    (f-expand (concat (symbol-name (cask-bundle-name bundle)) "-pkg.el") (cask-bundle-path bundle))))

(defun cask-path (bundle)
  "Return BUNDLE root path."
  (cask-bundle-path bundle))

(defun cask-file (bundle)
  "Return path to BUNDLE Cask-file."
  (f-expand "Cask" (cask-path bundle)))

(defun cask-files (bundle)
  "Return list of BUNDLE files with absolute path."
  (cask-with-file bundle
    (or (cask-bundle-files bundle)
        (cask-expand-files (cask-bundle-path bundle)))))

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
  (unless url
    (let ((mapping (assq name-or-alias cask-source-mapping)))
      (unless mapping
        (error "Unknown package archive: %s" name-or-alias))
      (setq name-or-alias (symbol-name (car mapping)))
      (setq url (cdr mapping))))
  (push (make-cask-source :name name-or-alias :url url) (cask-bundle-sources bundle)))

(defun cask-build (bundle)
  "Build BUNDLE Elisp files."
  (cask-with-file bundle
    (require 'bytecomp)
    (-each (cask-files bundle)
           (lambda (path)
             (when (and (f-file? path) (f-ext? path "el"))
               (byte-recompile-file path 'force 0))))))

(defun cask-clean-elc (bundle)
  "Remove BUNDLE Elisp byte compiled files."
  (cask-with-file bundle
    (-each (cask-files bundle)
           (lambda (path)
             (when (and (f-file? path) (f-ext? path "el"))
               (when (f-file? (concat path "c"))
                 (f-delete (concat path "c"))))))))

(defun cask-links (&optional bundle)
  "Return all links or BUNDLE links if specified.

If BUNDLE is specified return all links for this project.  If not,
return a list of all available links.

The list is a list of alist's where the key is the name of the
link, as a string and the value is the absolute path to the link."
  (if bundle
      (cask-with-file bundle
        (-map
         (lambda (file)
           (list (s-chop-suffix cask-link-suffix (f-filename file))
                 (f-canonical file)))
         (f-entries (cask-elpa-dir bundle)
                    (lambda (path)
                      (and (f-symlink? path) (s-ends-with? cask-link-suffix path))))))
    (when (f-file? cask-links-file)
      (read (f-read cask-links-file 'utf-8)))))

(defun cask-link-p (name)
  "Return true if link with NAME exists, false otherwise.

NAME can be either a symbol or a string."
  (if (symbolp name)
      (setq name (symbol-name name)))
  (-contains? (-map 'car (cask-links)) name))

(defun cask-link (bundle &optional name)
  "Link BUNDLE or add dev link with NAME.

If NAME is specified, add a local link to that link's path.  If
NAME is not specified, add the current project as a link."
  (cask-with-package bundle
    (if name
        (-if-let (source (cadr (--first (string= (car it) name) (cask-links))))
            (progn
              (cask-link-delete bundle name)
              (let ((target (f-expand (concat name cask-link-suffix) (cask-elpa-dir bundle))))
                (f-symlink source target)))
          (signal 'cask-no-such-link (list name)))
      (let ((name (cask-bundle-name bundle))
            (path (cask-bundle-path bundle))
            (links (cask-links)))
        (cask-write-links (push (list (symbol-name name) path) links))))))

(defun cask-link-delete (bundle &optional name)
  "Delete BUNDLE as link or BUNDLE link.

If NAME is specified, delete local link with NAME.  If no name,
delete current project as link."
  (cask-with-package bundle
    (if name
        (-each
         (f-glob (concat name "-*") (cask-elpa-dir bundle))
         (lambda (path)
           (f-delete path 'force)))
      (let ((name (cask-bundle-name bundle)))
        (if (cask-link-p name)
            (cask-write-links (--remove (string= name (car it)) (cask-links)))
          (signal 'cask-no-such-link (list name)))))))

(provide 'cask)

;;; cask.el ends here
