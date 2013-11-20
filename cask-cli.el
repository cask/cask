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

;; Avoid "Loading vc-git..." messages
(remove-hook 'find-file-hooks 'vc-find-file-hook)

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

(defun cask-cli/package ()
  (cask-cli--setup)
  (f-write-text (cask-package) 'utf-8 cask-package-file))

(defun cask-cli/install ()
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

(defun cask-cli/update ()
  (cask-cli--setup)
  (-when-let (upgrades (cask-update))
    (princ "Updated packages:\n")
    (-each upgrades 'cask-cli--print-upgrade)))

(defun cask-cli/init ()
  (cask-new-project cask-cli--path cask-cli--dev-mode))

(defun cask-cli/list ()
  (cask-cli--setup)
  (princ "### Dependencies ###\n\n")
  (princ (format "Runtime [%s]:\n" (length cask-runtime-dependencies)))
  (mapc 'cask-cli--print-dependency cask-runtime-dependencies)
  (if (> (length cask-runtime-dependencies) 0)
      (princ "\n"))
  (princ (format "Development [%s]:\n" (length cask-development-dependencies)))
  (mapc 'cask-cli--print-dependency cask-development-dependencies))

(defun cask-cli/version ()
  (cask-cli--setup)
  (princ (concat (cask-version) "\n")))

(defun cask-cli/info ()
  (cask-cli--setup)
  (let* ((info (cask-info))
         (name (cask-package-name info))
         (version (cask-package-version info))
         (description (cask-package-description info)))
    (princ (format "### %s (%s) ###" name version))
    (princ "\n\n")
    (princ description)
    (princ "\n")))

(defun cask-cli/help ()
  (commander-print-usage-and-exit))

(defun cask-cli/load-path ()
  (princ (concat (cask-load-path) "\n")))

(defun cask-cli/path ()
  (princ (concat (cask-path) "\n")))

(defun cask-cli/package-directory ()
  (princ (concat (cask-elpa-dir) "\n")))

(defun cask-cli/dev ()
  (setq cask-cli--dev-mode t))

(defun cask-cli/debug ()
  (setq debug-on-error t)
  (setq debug-on-entry t))

(defun cask-cli/outdated ()
  (cask-cli--setup)
  (-when-let (outdated (cask-outdated))
    (princ "Outdated packages:\n")
    (-each outdated 'cask-cli--print-upgrade)))

(defun cask-cli/set-path (path)
  (setq cask-cli--path path))

(commander
 (name "cask")
 (description "Emacs dependency management made easy")

 (default "install")

 (command "package" "Create -pkg.el file" cask-cli/package)
 (command "install" "Install dependencies" cask-cli/install)
 (command "update" "Update dependencies" cask-cli/update)
 (command "upgrade" "Upgrade Cask" cask-cli/upgrade)
 (command "exec [*]" "Execute command with correct dependencies" ignore)
 (command "init" "Create basic Cask file" cask-cli/init)
 (command "version" "Show the package version" cask-cli/version)
 (command "list" "List dependencies" cask-cli/list)
 (command "info" "Show info about this project" cask-cli/info)
 (command "help" "Display this help message" cask-cli/help)
 (command "load-path" "Print Emacs load-path (including package dependencies)" cask-cli/load-path)
 (command "path" "Print Emacs exec-path (including package bin path)" cask-cli/path)
 (command "package-directory" "Print package installation directory" cask-cli/package-directory)
 (command "outdated" "Show list of outdated packages" cask-cli/outdated)

 (option "-h, --help" "Display this help message" cask-cli/help)
 (option "--dev" "Run in dev mode" cask-cli/dev)
 (option "--debug" "Turn on debug output" cask-cli/debug)
 (option "--path <path>" "Run command in this path" cask-cli/set-path))

(provide 'cask-cli)

;;; cask-cli.el ends here
