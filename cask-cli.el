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

(defvar cask--cli-bundle-cache nil)

(defun cask--cli-bundle ()
  "Setup in `cask-cli--path' and return bundle."
  (or cask--cli-bundle-cache
      (setq cask--cli-bundle-cache (cask-setup cask-cli--path))))

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


;;;; Commands

(defun cask-cli/package ()
  "Write a `define-package' file.

The file is written to the Cask project root path with name
{project-name}-pkg.el."
  (f-write-text (cask-define-package-string (cask--cli-bundle)) 'utf-8
                (cask-define-package-file (cask--cli-bundle))))

(defun cask-cli/install ()
  "Install all packages specified in the Cask-file.

The dependencies to packages are also installed.  If a package
already is installed, it will not be installed again."
  (condition-case err
      (cask-install (cask--cli-bundle))
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
  (-when-let (upgrades (cask-update (cask--cli-bundle)))
    (princ "Updated packages:\n")
    (-each upgrades 'cask-cli--print-upgrade)))

(defun cask-cli/init ()
  "Initialize the current directory with a Cask-file.

By default, the created Cask-file will be for an Emacs
configuration.  If the --dev option is specified, the Cask-file
will be for an Emacs package."
  (cask-caskify cask-cli--path cask-cli--dev-mode))

(defun cask-cli/list ()
  "List this package dependencies."
  (let ((runtime-dependencies (cask-runtime-dependencies (cask--cli-bundle)))
        (development-dependencies (cask-development-dependencies (cask--cli-bundle))))
    (princ "### Dependencies ###\n\n")
    (princ (format "Runtime [%s]:\n" (length runtime-dependencies)))
    (mapc 'cask-cli--print-dependency runtime-dependencies)
    (if (> (length runtime-dependencies) 0)
        (princ "\n"))
    (princ (format "Development [%s]:\n" (length development-dependencies)))
    (mapc 'cask-cli--print-dependency development-dependencies)))

(defun cask-cli/version ()
  "Print version for the current project."
  (princ (concat (cask-package-version (cask--cli-bundle)) "\n")))

(defun cask-cli/info ()
  "Show info about the current package."
  (let ((name (cask-package-name (cask--cli-bundle)))
        (version (cask-package-version (cask--cli-bundle)))
        (description (cask-package-description (cask--cli-bundle))))
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
  (princ (concat (cask-load-path (cask--cli-bundle)) "\n")))

(defun cask-cli/exec-path ()
  "Print `exec-path' for all packages and dependencies.

A dependency will be included in this list of the package has a
directory called bin in the root directory.

The output is formatted as a colon path."
  (princ (concat (cask-exec-path (cask--cli-bundle)) "\n")))

(defun cask-cli/package-directory ()
  "Print current package installation directory."
  (princ (concat (cask-elpa-dir (cask--cli-bundle)) "\n")))

(defun cask-cli/outdated ()
  "Print list of outdated packages.

That is packages that have a more recent version available for
installation."
  (-when-let (outdated (cask-outdated (cask--cli-bundle)))
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
