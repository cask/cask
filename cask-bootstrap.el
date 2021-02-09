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

(defvar cask-directory)


(defconst cask-bootstrap-dir
  (expand-file-name
   (locate-user-emacs-file
    (format ".cask/%s.%s/bootstrap" emacs-major-version emacs-minor-version)))
  "Path to Cask bootstrap directory.")

(defconst cask-bootstrap-packages
  '(s dash f commander git epl shut-up cl-lib package-build eieio ansi)
  "List of bootstrap packages required by this file.")

(unless (require 'package nil :noerror)
  (require 'package (expand-file-name "package-legacy" cask-directory)))

(when (version< emacs-version "25.1")
  ;; Use vendored package-build package (and package-recipe) because its newer
  ;; versions require Emacs25.1+
  (let (package-archives
        package-alist
        package-archive-contents
        (package-user-dir cask-bootstrap-dir))
    (unless package--initialized
      (package-initialize))

    (unless (package-installed-p 'cl-lib)
      (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
      (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
      (package-refresh-contents)
      (package-install 'cl-lib)))
  (require 'package-recipe (expand-file-name "package-recipe-legacy" cask-directory))
  (require 'package-build (expand-file-name "package-build-legacy" cask-directory))
  (delq 'cl-lib cask-bootstrap-packages)
  (delq 'package-build cask-bootstrap-packages)
  (delq 'eieio cask-bootstrap-packages))

(let ((orig-load-path load-path))
  (unwind-protect
      (let (package-archives
            package-alist
            package-archive-contents
            (package-user-dir cask-bootstrap-dir))
        (package-initialize)
        (mapc
         (lambda (package)
           (condition-case nil
               (require package)
             (error
              (unless package-archives
                (add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
                (add-to-list 'package-archives (cons "melpa" "https://stable.melpa.org/packages/"))
                (package-refresh-contents))
              (package-install package)
              (require package))))
         cask-bootstrap-packages))
    (setq load-path orig-load-path)))

(provide 'cask-bootstrap)

;;; cask-bootstrap.el ends here
