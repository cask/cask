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

(eval-when-compile
  (defvar cask-directory))

(defconst cask-bootstrap-dir
  (expand-file-name
   (locate-user-emacs-file (format ".cask/%s/bootstrap" emacs-version)))
  "Path to Cask bootstrap directory.")

(defconst cask-bootstrap-packages '(s dash f commander git epl shut-up)
  "List of bootstrap packages required by this file.")

(unless (require 'package nil :noerror)
  (require 'package (expand-file-name "package-legacy" cask-directory)))

(let ((orig-load-path load-path))
  (unwind-protect
      (let (package-archives
            package-alist
            package-archive-contents
            (package-user-dir cask-bootstrap-dir))
        (package-initialize)
        (condition-case nil
            (mapc 'require cask-bootstrap-packages)
          (error
           (add-to-list 'package-archives (cons "gnu" "http://elpa.gnu.org/packages/"))
           (add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
           (package-refresh-contents)
           (mapc
            (lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
            cask-bootstrap-packages)
           (mapc 'require cask-bootstrap-packages))))
    (setq load-path orig-load-path)))

(provide 'cask-bootstrap)

;;; cask-bootstrap.el ends here
