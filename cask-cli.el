;;; cask-cli.el --- Cask: CLI interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2014 Johan Andersson

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

(when noninteractive
  (shut-up-silence-emacs))

(defconst cask-cli--table-padding 10
  "Number of spaces to pad with when printing table.")

(defvar cask-cli--dev-mode nil
  "If Cask should run in dev mode or not.")

(defvar cask-cli--silent nil
  "If Cask should suppress logging.")

(defvar cask-cli--path default-directory
  "Cask commands will execute in this path.")

(defvar cask-cli--bundle-cache nil)

(defun cask-cli--bundle ()
  "Setup in `cask-cli--path' and return bundle."
  (or cask-cli--bundle-cache
      (setq cask-cli--bundle-cache (cask-setup cask-cli--path))))

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

(defun cask-cli--print-table (table)
  "Print TABLE, which is a list of alist's."
  (let ((max (length (--max-by (> (length it)
                                  (length other))
                               (-map 'car table)))))
    (-each table
      (lambda (row)
        (let ((key (car row)) (value (cadr row)))
          (princ (s-pad-right (+ max cask-cli--table-padding) " " key))
          (princ (concat value "\n")))))))


;;;; Commands

(defmacro cask-cli/with-handled-errors (&rest body)
  "Evaluate BODY and handle errors accordingly."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (cask-missing-dependencies
      (let ((missing-dependencies (cdr err)))
        (error "Some dependencies were not available: %s"
               (->> missing-dependencies
                 (-map #'cask-dependency-name)
                 (-map #'symbol-name)
                 (s-join ", ")))))
     (cask-failed-initialization
      (let* ((data (cdr err))
             (message (error-message-string (nth 0 data)))
             (output (nth 1 data)))
        (error "Package initialization failed: %s\nOutput:\n%s"
               message output)))
     (cask-failed-installation
      (let* ((data (cdr err))
             (dependency (cask-dependency-name (nth 0 data)))
             (message (error-message-string (nth 1 data)))
             (output (nth 2 data)))
        (if dependency
            (error "Dependency %s failed to install: %s\nOutput:\n%s"
                   dependency message output)
          (error "Package installation failed: %s\nOutput:\n%s"
                 message output))))))

(defun cask-cli/pkg-file ()
  "Write a `define-package' file.

The file is written to the Cask project root path with name
{project-name}-pkg.el."
  (f-write-text (cask-define-package-string (cask-cli--bundle)) 'utf-8
                (cask-define-package-file (cask-cli--bundle))))

(defun cask-cli/install ()
  "Install all packages specified in the Cask-file.

The dependencies to packages are also installed.  If a package
already is installed, it will not be installed again."
  (cask-cli/with-handled-errors
    (cask-install (cask-cli--bundle))))

(defun cask-cli/upgrade-cask ()
  "Upgrade Cask itself and its dependencies.

This command requires that Cask is installed using Git and that
Git is available in `exec-path'."
  (unless (f-exists? (f-expand ".no-upgrade" cask-directory))
    (unwind-protect
        (progn
          (epl-change-package-dir cask-bootstrap-dir)
          (epl-initialize)
          (epl-add-archive "gnu" "https://elpa.gnu.org/packages/")
          (epl-add-archive "melpa" "https://melpa.org/packages/")
          (epl-refresh)
          (epl-upgrade))
      (epl-reset))
    (require 'git)
    (let ((git-repo cask-directory))
      (if (s-present? (git-run "status" "--porcelain"))
          (error "Cannot update Cask because of dirty tree")
        (git-pull)))))

(defun cask-cli/exec (&rest args)
  "Execute ARGS with correct `exec-path' and `load-path'.")

(defun cask-cli/update ()
  "Update package versions.

All packages that are specified in the Cask-file will be updated
including their dependencies."
  (cask-cli/with-handled-errors
    (-when-let (upgrades (cask-update (cask-cli--bundle)))
      (princ "Updated packages:\n")
      (-each upgrades 'cask-cli--print-upgrade))))

(defun cask-cli/init ()
  "Initialize the current directory with a Cask-file.

By default, the created Cask-file will be for an Emacs
configuration.  If the --dev option is specified, the Cask-file
will be for an Emacs package."
  (cask-caskify (cask-cli--bundle) cask-cli--dev-mode))

(defun cask-cli/list ()
  "List this package dependencies."
  (let ((runtime-dependencies (cask-runtime-dependencies (cask-cli--bundle)))
        (development-dependencies (cask-development-dependencies (cask-cli--bundle))))
    (princ "### Dependencies ###\n\n")
    (princ (format "Runtime [%s]:\n" (length runtime-dependencies)))
    (mapc 'cask-cli--print-dependency runtime-dependencies)
    (if (> (length runtime-dependencies) 0)
        (princ "\n"))
    (princ (format "Development [%s]:\n" (length development-dependencies)))
    (mapc 'cask-cli--print-dependency development-dependencies)))

(defun cask-cli/version ()
  "Print version for the current project."
  (princ (concat (cask-package-version (cask-cli--bundle)) "\n")))

(defun cask-cli/info ()
  "Show info about the current package."
  (let ((name (cask-package-name (cask-cli--bundle)))
        (version (cask-package-version (cask-cli--bundle)))
        (description (cask-package-description (cask-cli--bundle))))
    (princ (format "### %s (%s) ###" name version))
    (princ "\n\n")
    (princ description)
    (princ "\n")))

(defun cask-cli/help (&optional command-name)
  "Display usage information or documentation for COMMAND-NAME."
  (if command-name
      (commander-print-usage-for-and-exit command-name)
    (commander-print-usage-and-exit)))

(defun cask-cli/load-path ()
  "Print `load-path' for all packages and dependencies.

The output is formatted as a colon path."
  (princ (concat (s-join path-separator (cask-load-path (cask-cli--bundle))) "\n")))

(defun cask-cli/exec-path ()
  "Print `exec-path' for all packages and dependencies.

A dependency will be included in this list of the package has a
directory called bin in the root directory.

The output is formatted as a colon path."
  (princ (concat (s-join path-separator (cask-exec-path (cask-cli--bundle))) "\n")))

(defmacro cask-cli--with-package-path (&rest body)
  "Execute BODY with `load-path' set according to the project."
  (declare (debug t))
  `(let ((load-path
          (cask-load-path (cask-cli--bundle))))
     (add-to-list 'load-path
                  cask-cli--path)
     ,@body))

(defun cask-cli/eval (form)
  "Eval FORM with the `load-path' set according to the project."
  (cask-cli--with-package-path
   (eval (read form))))

(defun cask-cli/package-directory ()
  "Print current package installation directory."
  (princ (concat (cask-elpa-path (cask-cli--bundle)) "\n")))

(defun cask-cli/outdated ()
  "Print list of outdated packages.

That is packages that have a more recent version available for
installation."
  (-when-let (outdated (cask-outdated (cask-cli--bundle)))
    (princ "Outdated packages:\n")
    (-each outdated 'cask-cli--print-upgrade)))

(defun cask-cli/files ()
  "Print list of files specified in the files directive.

If no files directive or no files, do nothing."
  (-each (cask-files (cask-cli--bundle))
    (lambda (file)
      (princ (concat file "\n")))))

(defun cask-cli/build ()
  "Build all Elisp files in the files directive."
  (cask-build (cask-cli--bundle)))

(defun cask-cli/clean-elc ()
  "Remove all byte compiled Elisp files in the files directive."
  (cask-clean-elc (cask-cli--bundle)))

(defun cask-cli/link (&optional command-or-name arg)
  "Manage links.

A link is just that, a symbolic link.  The purpose of the link
command is that you should be able to work with local
dependencies.

For example, let's say you are developing an Emacs package that
depends on f.el. Consider what happens if you need to extend f.el
with some function that your package requires.

With the link command, you can checkout f.el locally, add it as a
link in your local package.  That means that when you require
f.el, you will require the local package instead of the one
fetched from the ELPA mirror.  Now you add the desired function
to f.el and use your library to try it out.

COMMAND-OR-NAME can be one of: delete, list or a link name.
ARG is sent to some of the commands.

Commands:

 $ cask link list

  List all project links.

 $ cask link name path

  Add local link with NAME to PATH.

 $ cask link delete name

  Delete local link with NAME."
  (cond ((string= command-or-name "delete")
         (cask-link-delete (cask-cli--bundle) (intern arg)))
        ((string= command-or-name "list")
         (cask-cli--print-table
          (cask-links (cask-cli--bundle))))
        ((stringp command-or-name)
         (cask-link (cask-cli--bundle) (intern command-or-name) arg))
        (t
         (cask-cli/help "link"))))

(defun cask-cli/package (&optional target-dir)
  "Build package and put in TARGET-DIR or dist if not specified."
  (cask-package (cask-cli--bundle) target-dir))

(defun cask-cli/emacs ()
  "Execute emacs with the appropriate environment.")


;;;; Options

(require 'url)

(defun cask-cli/cask-proxy (host)
  "Set Emacs proxy for HTTP and HTTPS to HOST."
  (cask-cli/cask-http-proxy host)
  (cask-cli/cask-https-proxy host))

(defun cask-cli/cask-http-proxy (host)
  "Set Emacs proxy for HTTP to HOST."
  (push (cons "http" host) url-proxy-services))

(defun cask-cli/cask-https-proxy (host)
  "Set Emacs proxy for HTTPS to HOST."
  (push (cons "https" host) url-proxy-services))

(defun cask-cli/cask-no-proxy (host)
  "Set Emacs no-proxy to HOST."
  (push (cons "no_proxy" host) url-proxy-services))

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

(defun cask-cli/verbose ()
  "Be verbose and show debug output."
  (setq shut-up-ignore t))

;;; TODO: Allow tracing an arbitrary prefix.
(defun cask-cli/trace ()
  "Trace cask function calls."
  (require 'cask-trace (expand-file-name "cask-trace" cask-directory))
  (cask-trace-prefix "cask-")
  (setq cask-trace-entry-p t)
  (setq cask-trace-exit-p t))

(defun cask-cli/silent ()
  "Be silent and do not print anything."
  (setq cask-cli--silent t))


;;;; Commander schedule

(commander
 (name "cask")
 (description "Emacs dependency management made easy")

 (default "install")

 (command "pkg-file" cask-cli/pkg-file)
 (command "install" cask-cli/install)
 (command "update" cask-cli/update)
 (command "upgrade" cask-cli/upgrade-cask)
 (command "upgrade-cask" cask-cli/upgrade-cask)
 (command "exec [*]" cask-cli/exec)
 (command "init" cask-cli/init)
 (command "version" cask-cli/version)
 (command "list" cask-cli/list)
 (command "info" cask-cli/info)
 (command "help [command]" cask-cli/help)
 (command "load-path" cask-cli/load-path)
 (command "exec-path" cask-cli/exec-path)
 (command "eval <form>" cask-cli/eval)
 (command "path" cask-cli/exec-path)
 (command "package-directory" cask-cli/package-directory)
 (command "outdated" cask-cli/outdated)
 (command "files" cask-cli/files)
 (command "build" cask-cli/build)
 (command "clean-elc" cask-cli/clean-elc)
 (command "link [*]" cask-cli/link)
 (command "package [target-dir]" cask-cli/package)
 (command "emacs [*]" cask-cli/emacs)

 (option "--proxy <host>" cask-cli/cask-proxy)
 (option "--http-proxy <host>" cask-cli/cask-http-proxy)
 (option "--https-proxy <host>" cask-cli/cask-https-proxy)
 (option "--no-proxy <host>" cask-cli/cask-no-proxy)

 (option "--version" cask-cli/cask-version)
 (option "-h [command], --help [command]" cask-cli/help)
 (option "--dev" cask-cli/dev)
 (option "--debug" cask-cli/debug)
 (option "--path <path>" cask-cli/set-path)
 (option "--verbose" cask-cli/verbose)
 (option "--trace" cask-cli/trace)
 (option "--silent" cask-cli/silent))

(provide 'cask-cli)

;;; cask-cli.el ends here
