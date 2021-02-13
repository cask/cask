;;; cask-bootstrap.el --- Cask: Bootstrap internal dependencies  -*- lexical-binding: t; -*-

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

;; Bootstrap Cask's internal dependencies.

;;; Code:

(require 'package)

(defvar cask-directory)


(defconst cask-bootstrap-dir
  (expand-file-name
   (locate-user-emacs-file
    (format ".cask/%s.%s/bootstrap" emacs-major-version emacs-minor-version)))
  "Path to Cask bootstrap directory.")

;; Don't change global `load-path' via `package-install'.
;; We need `s' or something else dependency package, But the
;; load-path has to be the user's, otherwise it hides the
;; dependency issues of the user's package.
(let ((load-path load-path)
      (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://stable.melpa.org/packages/")))
      package-alist
      package-archive-contents
      (package-user-dir cask-bootstrap-dir)
      (deps '(s dash f commander git epl shut-up cl-lib cl-generic
                package-build eieio ansi)))
  (when (version< emacs-version "25.1")
    ;; Builtin gnutls on Emacs 24 was used incorrectly, and
    ;; cannot connect to melpa.  Use external openssl instead.
    (require 'tls)
    (defvar tls-program)
    (setq tls-program '("openssl s_client -connect %h:%p -no_ssl3 -no_ssl2 -ign_eof"))
    (defun gnutls-available-p () nil)

    (unless package--initialized
      (package-initialize))

    (unless (package-installed-p 'cl-lib)
      (package-refresh-contents)
      (package-install 'cl-lib))

    ;; Use vendored package-build package (and package-recipe)
    ;; because its newer versions require Emacs25.1+
    (require 'package-recipe (expand-file-name "package-recipe-legacy" cask-directory))
    (require 'package-build (expand-file-name "package-build-legacy" cask-directory))
    (delq 'cl-lib deps)
    (delq 'package-build deps)
    (delq 'eieio deps))

  (package-initialize)
  (mapc
   (lambda (package)
     (condition-case nil
         (require package)
       (error
        (unless package-archives
          (package-refresh-contents))
        (package-install package)
        (require package))))
   deps))

(provide 'cask-bootstrap)

;;; cask-bootstrap.el ends here
