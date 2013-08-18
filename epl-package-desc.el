;;; epl-package-desc.el --- Emacs Package Library: package-desc backend -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Keywords: convenience
;; URL: http://github.com/rejeep/cask.el

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

;; EPL implementation based on package-desc.

;;; Code:

;;;; Compatibility check

(unless (require 'package nil :no-error)
  ;; Try to require package.el to check its API version
  (epl-error "Library package.el missing"))

(unless (fboundp 'package-desc-create)
  ;; The package-desc structure is missing, hence indicate that this API cannot
  ;; be loaded
  (epl-error "Legacy package.el API detected"))


;;;; Requirements

;; We can safely used `cl-lib' here, because it predated the defstruct-based
;; package.el
(require 'cl-lib)                       ; For cl-defstruct


;;;; Package objects

(cl-defstruct (epl-requirement
               (:constructor epl-requirement-create))
  "Structure describing a requirement.
Slots:

`name' The name of the required package, as symbol.

`version' The version of the required package, as version list."
  name
  version)

;; `package-desc' provides all necessary information, so let's treat it as
;; opaque `epl-package' struct and simply alias all necessary definitions.

(defalias 'epl-package-p 'package-desc-p
  "Determine whether OBJ is a package object.")

(defalias 'epl-package-name 'package-desc-name
  "Get the name from a package object, as symbol.")

(defalias 'epl-package-summary 'package-desc-summary
  "Get the summary from a package object, as string.")

(defalias 'epl-package-version 'package-desc-version
  "Get the version from a package object, as version list.")

(defun epl-requirement--from-req (req)
  "Create a `epl-requirement' from a `package-desc' REQ."
  (epl-requirement-create :name (car req)
                          :version (cadr req)))

(defun epl-package-requirements (package)
  "Get the requirements from a PACKAGE.

Return a list of requirements, as `epl-requirement' objects."
  (mapcar #'epl-requirement--from-req (package-desc-reqs package)))

(defun epl-package-from-buffer (&optional buffer)
  "Create a `epl-package' object from a BUFFER.

Parse the package metadata of BUFFER and return a corresponding
`epl-package' object."
  (with-current-buffer buffer
    (package-buffer-info)))


;;;; Package system management

(defvar epl--load-path-before-initialize)

(defun epl-reset ()
  "Reset the package system.

Clear the list of installed and available packages, the list of
package archives and reset the package directory."
  (setq package-alist nil
        package-archives nil
        package-archive-contents nil
        load-path epl--load-path-before-initialize)
  (epl-change-package-dir (epl-default-package-dir)))


;;;; Package database access

(defun epl-installed-packages ()
  "Get all installed packages.

Return a list of package objects."
  (mapcar #'cadr package-alist))

(defun epl-find-installed-package (name)
  "Find an installed package by NAME.

NAME is a package name, as symbol.

Return the installed package as package object, or nil if no
package with NAME is installed."
  (cadr (assq name package-alist)))

(defun epl-available-packages ()
  "Get all packages available for installed.

Return a list of package objects."
  (mapcar #'cadr package-archive-contents))

(defun epl-package-->=(pkg1 pkg2)
  "Determine whether PKG1 is before PKG2 by version."
  (not (version-list-< (epl-package-version pkg1)
                       (epl-package-version pkg2))))

(defun epl-find-available-packages (name)
  "Find  available packages for NAME.

NAME is a package name, as symbol.

Return a list of packages for NAME, sorted by version number in
descending order."
  ;; Sort modifies the list, hence we need to copy first to avoid messing around
  ;; in the archive contents
  (let ((pkgs (cdr (assq name package-archive-contents))))
    (when pkgs
      (sort (copy-sequence pkgs) #'epl-package-->=))))

(cl-defstruct (epl-upgrade
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
    (package-install package)))

(defun epl-package-delete (package)
  "Delete a PACKAGE.

PACKAGE is a package object referring to the package which shall
be deleted.

The package directory is deleted, and the package is removed from
the list of available packages.  However, if files from PACKAGE
were loaded they are not unloaded by deleting the package."
  ;; package-delete allows for packages being trashed instead of fully deleted.
  ;; Let's prevent his silly behavior
  (let ((delete-by-moving-to-trash nil))
    (package-delete package)))

(provide 'epl-package-desc)

;;; epl-package-desc.el ends here
