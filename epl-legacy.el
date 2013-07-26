;;; epl-package-desc.el --- Emacs Package Library: package-desc backend -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Keywords: convenience
;; URL: http://github.com/rejeep/carton

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

;; EPL implementation based on the legacy, non-defstruct package.el.

;;; Code:

;;;; Compatibility check

(unless (require 'package nil :no-error)
  ;; Load the legacy package.el from Emacs 23
  (require 'package (expand-file-name "package-legacy" epl-directory)))

(when (fboundp 'package-desc-create)
  ;; The package-desc structure is present, hence indicate that this API cannot
  ;; be loaded
  (signal 'epl-error "Modern package.el API detected"))


;;;; Dependencies
(eval-when-compile
  (require 'cl))                        ; For `defstruct' and `destructuring-bind'


;;;; Package objects

(defstruct (epl-requirement
               (:constructor epl-requirement-create))
  "Structure describing a requirement.
Slots:

`name' The name of the required package, as symbol.

`version' The version of the required package, as version list."
  name
  version)

(defstruct (epl-package
            (:constructor epl-package-create))
  "Structure representing a package.
Slots:

`name' The package name.

`summary' The package summary.

`version' The package version.

`requirements' The requirements."
  name
  summary
  version-string
  requirements)

(defun epl-requirement--from-req (req)
  "Create a `epl-requirement' from a `package-desc' REQ."
  (let ((version (cadr req)))
    (epl-requirement-create :name (car req)
                            :version (if (listp version) version
                                       (version-to-list version)))))

(defun epl-package-from-buffer (&optional buffer)
  "Create a `epl-package' object from a BUFFER.

Parse the package metadata of BUFFER and return a corresponding
`epl-package' object."
  (with-current-buffer buffer
    (destructuring-bind (name requires desc version _)
        (append (package-buffer-info) nil) ; `destructing-bind' doesn't like
                                           ; vectors
      (epl-package-create
       :name name
       :summary desc
       :version (version-to-list version)
       :requirements (mapcar #'epl-requirement--from-req requires)))))


;;;; Package system management

(defun epl-reset ()
  "Reset the package system.

Clear the list of installed and available packages, the list of
package archives and reset the package directory."
  (setq package-alist nil
        package-obsolete-alist nil
        package-archives nil
        package-archive-contents nil)
  (epl-change-package-dir (epl-default-package-dir)))


;;;; Package database access

(defun epl-package--from-package-alist-entry (entry)
  "Create a `epl-package' from an item in `package-alist'."
  (let ((name (car entry))
        (info (cdr entry)))
    (destructuring-bind (version reqs docstring) (append info nil)
      (epl-package-create
       :name name
       :summary docstring
       :version (if (listp version) version
                  (version-to-list version))
       :requirements (mapcar #'epl-requirement--from-req reqs)))))

(defun epl-installed-packages ()
  "Get all installed packages.

Return a list of package objects."
  (mapcar #'epl-package--from-package-alist-entry package-alist))

(defun epl-find-installed-package (name)
  "Find an installed package by NAME.

NAME is a package name, as symbol.

Return the installed package as package object, or nil if no
package with NAME is installed."
  (let ((entry (assq name package-alist)))
    (when entry
      (epl-package--from-package-alist-entry entry))))

(defun epl-available-packages ()
  "Get all packages available for installed.

Return a list of package objects."
  (mapcar #'cadr package-archive-contents))

(defun epl-find-available-package (name)
  "Find an available package by NAME.

NAME is a package name, as symbol.

Return the package as package object, or nil, if no package with
NAME is available."
  (cadr (assq name package-archive-contents)))

(defstruct (epl-upgrade
            (:constructor epl-upgrade-create))
  "Structure describing an upgradable package.
Slots:

`installed' The installed package

`available' The package available for installation."
  installed
  available)


;;;; Package operations

(defun epl-package-install (package &optional force)
  "Install a PACKAGE.

PACKAGE is a package object.

If FORCE is given and non-nil, install PACKAGE even if it is
already installed."
  (when (or force (not (epl-package-installed-p package)))
    (package-install (epl-package-name package))))

(defun epl-package-delete (package)
  "Delete a PACKAGE.

PACKAGE is a package object to delete."
  (package-delete (epl-package-name package)
                  (epl-package-version-string package)))

(provide 'epl-package-desc)

;;; epl-package-desc.el ends here
