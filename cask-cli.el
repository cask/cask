;;; cask-cli.el --- Cask: CLI interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/cask/cask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; CLI interface of Cask.

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
(require 'cask (expand-file-name "cask" cask-directory))

(defvar cask-cli--dev-mode nil
  "If Cask should run in dev mode or not.")

(defvar cask-cli--path default-directory
  "Cask commands will execute in this path.")

(defun cask-cli--find-unbalanced-parenthesis ()
  (with-temp-buffer
    (insert (f-read-text cask-file 'utf-8))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (check-parens)
          nil)
      (error (cask-current-source-position)))))

(defun cask-cli--exit-error (err)
  (let ((type (car err))
        (data (cdr err))
        pos msg)
    (if (eq type 'end-of-file)
        ;; In case of premature end of file, try hard to find the real
        ;; position, by scanning for unbalanced parenthesis
        (setq pos (or (cask-cli--find-unbalanced-parenthesis) (cadr err))
              msg "End of file while reading (possible unbalanced parenthesis)")
      ;; For other types of error, check whether the error has a position, and
      ;; print it.  Otherwise just print the error like Emacs would do
      (when (cask-source-position-p (car data))
        (setq pos (car data))
        ;; Strip the position from the error data
        (setq data (cdr data)))
      (setq msg (error-message-string (cons type data))))
    (if pos
        (message "%s:%s:%s: %s" cask-file (cask-source-position-line pos)
                 (cask-source-position-column pos) msg)
      (message "%s: %s" cask-file msg)))
  (kill-emacs 1))

(defun cask-cli--setup ()
  (condition-case err
      (cask-setup cask-cli--path)
    (end-of-file
     (cask-cli--exit-error err))
    (invalid-read-syntax
     (cask-cli--exit-error err))))

(defun cask-cli--print-dependency (dependency)
  (let ((name (cask-dependency-name dependency))
        (version (cask-dependency-version dependency)))
    (princ
     (if version
         (format " - %s (%s)" name version)
       (format " - %s" name)))
    (princ "\n")))

(defun cask-cli--print-upgrade (upgrade)
  (princ
   (format
    "%s %s -> %s\n"
    (epl-package-name (epl-upgrade-installed upgrade))
    (epl-package-version-string (epl-upgrade-installed upgrade))
    (epl-package-version-string (epl-upgrade-available upgrade)))))

(defmacro cask-cli--with-setup (&rest body)
  "Setup Cask and yield BODY with `cask-bundle' object as `it'."
  `(let ((it (cask-setup cask-cli--path)))
     ,@body))


;;;; Commands

(defun cask-cli/package ()
  "Write a `define-package' file.

The file is written to the Cask project root path with name
{project-name}-pkg.el."
  (cask-cli--with-setup
   (f-write-text (cask-define-package-string it) 'utf-8
                 (cask-define-package-file it))))

(defun cask-cli/install ()
  "Install all packages specified in the Cask-file.

The dependencies to packages are also installed.  If a package
already is installed, it will not be installed again."
  (cask-cli--setup)
  (condition-case err
      (cask-install)
    (cask-missing-dependencies
     (let ((missing-dependencies (cdr err)))
       (error "Some dependencies were not available: %s"
              (->> missing-dependencies
                (-map #'cask-dependency-name)
                (-map #'symbol-name)
                (s-join ", ")))))
    (cask-failed-installation
     (let* ((data (cdr err))
            (dependency (cask-dependency-name (car data)))
            (message (error-message-string (cdr data))))
       (error "Dependency %s failed to install: %s" dependency message)))))

(defun cask-cli/upgrade ()
  "Upgrade Cask itself and its dependencies.

This command requires that Cask is installed using Git and that
Git is available in `exec-path'."
  (unwind-protect
      (progn
        (epl-change-package-dir cask-bootstrap-dir)
        (epl-initialize)
        (epl-add-archive "gnu" "http://elpa.gnu.org/packages/")
        (epl-add-archive "melpa" "http://melpa.milkbox.net/packages/")
        (epl-refresh)
        (epl-upgrade))
    (epl-reset))
  (require 'git)
  (let ((git-repo cask-directory))
    (if (s-present? (git-run "status" "--porcelain"))
        (error "Cannot update Cask because of dirty tree")
      (git-pull))))

(defun cask-cli/exec (&rest args)
  "Execute ARGS with correct `exec-path' and `load-path'.")

(defun cask-cli/update ()
  "Update package version.

All packages that are specified in the Cask-file will be updated
including their dependencies."
  (cask-cli--setup)
  (-when-let (upgrades (cask-update))
    (princ "Updated packages:\n")
    (-each upgrades 'cask-cli--print-upgrade)))

(defun cask-cli/init ()
  "Initialize the current directory with a Cask-file."
  (cask-new-project cask-cli--path cask-cli--dev-mode))

(defun cask-cli/list ()
  "List this package dependencies."
  (cask-cli--setup)
  (princ "### Dependencies ###\n\n")
  (princ (format "Runtime [%s]:\n" (length cask-runtime-dependencies)))
  (mapc 'cask-cli--print-dependency cask-runtime-dependencies)
  (if (> (length cask-runtime-dependencies) 0)
      (princ "\n"))
  (princ (format "Development [%s]:\n" (length cask-development-dependencies)))
  (mapc 'cask-cli--print-dependency cask-development-dependencies))

(defun cask-cli/version ()
  "Print version for the current project."
  (cask-cli--with-setup
   (princ (concat (cask-package-version it) "\n"))))

(defun cask-cli/info ()
  "Show info about the current package."
  (cask-cli--setup)
  (let* ((info (cask-info))
         (name (plist-get info :name))
         (version (plist-get info :version))
         (description (plist-get info :description)))
    (princ (format "### %s (%s) ###" name version))
    (princ "\n\n")
    (princ description)
    (princ "\n")))

(defun cask-cli/help ()
  "Display usage information."
  (commander-print-usage-and-exit))

(defun cask-cli/load-path ()
  "Print `load-path' for all packages and dependencies.

The output is formatted as a colon path."
  (cask-cli--with-setup
   (princ (concat (cask-load-path it) "\n"))))

(defun cask-cli/exec-path ()
  "Print `exec-path' for all packages and dependencies.

A dependency will be included in this list of the package has a
directory called bin in the root directory.

The output is formatted as a colon path."
  (cask-cli--with-setup
   (princ (concat (cask-exec-path it) "\n"))))

(defun cask-cli/package-directory ()
  "Print current package installation directory."
  (cask-cli--with-setup
   (princ (concat (cask-elpa-dir it) "\n"))))

(defun cask-cli/outdated ()
  "Print list of outdated packages.

That is packages that have a more recent version available for
installation."
  (cask-cli--setup)
  (-when-let (outdated (cask-outdated))
    (princ "Outdated packages:\n")
    (-each outdated 'cask-cli--print-upgrade)))


;;;; Options

(defun cask-cli/cask-version ()
  "Print Cask's version."
  (princ (concat (cask-version) "\n"))
  (kill-emacs 0))

(defun cask-cli/set-path (path)
  "Run command in this PATH instead of in `default-directory'."
  (setq cask-cli--path path))

(defun cask-cli/dev ()
  "Run in dev mode."
  (setq cask-cli--dev-mode t))

(defun cask-cli/debug ()
  "Turn on debug output."
  (setq debug-on-error t))


;;;; Commander schedule

(commander
 (name "cask")
 (description "Emacs dependency management made easy")

 (default "install")

 (command "package" cask-cli/package)
 (command "install" cask-cli/install)
 (command "update" cask-cli/update)
 (command "upgrade" cask-cli/upgrade)
 (command "exec [*]" cask-cli/exec)
 (command "init" cask-cli/init)
 (command "version" cask-cli/version)
 (command "list" cask-cli/list)
 (command "info" cask-cli/info)
 (command "help" cask-cli/help)
 (command "load-path" cask-cli/load-path)
 (command "exec-path" cask-cli/exec-path)
 (command "path" cask-cli/exec-path)
 (command "package-directory" cask-cli/package-directory)
 (command "outdated" cask-cli/outdated)

 (option "--version" cask-cli/cask-version)
 (option "-h, --help" cask-cli/help)
 (option "--dev" cask-cli/dev)
 (option "--debug" cask-cli/debug)
 (option "--path <path>" cask-cli/set-path))

(provide 'cask-cli)

;;; cask-cli.el ends here
