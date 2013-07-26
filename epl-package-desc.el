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

;; EPL implementation based on package-desc.

;;; Code:

;;;; Compatibility check

(unless (require 'package nil :no-error)
  ;; Try to require package.el to check its API version
  (epl-error "package.el missing"))

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

(defun epl-reset ()
  "Reset the package system.

Clear the list of installed and available packages, the list of
package archives and reset the package directory."
  (setq package-alist nil
        package-archives nil
        package-archive-contents nil)
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

(defun epl-find-available-package (name)
  "Find an available package by NAME.

NAME is a package name, as symbol.

Return the package as package object, or nil, if no package with
NAME is available."
  (cadr (assq name package-archive-contents)))

(cl-defstruct (epl-upgrade
               (:constructor epl-upgrade-create))
  "Structure describing an upgradable package.
Slots:

`installed' The installed package

`available' The package available for installation."
  installed
  available)

(defun epl-find-upgrades (&optional packages)
  "Find all upgradable PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

Return a list of `epl-upgrade' objects describing all upgradable
packages."
  (let ((packages (or packages (epl-installed-packages)))
        upgrades)
    (dolist (pkg packages)
      (let* ((version (epl-package-version pkg))
             (name (epl-package-name pkg))
             (available-pkg (epl-find-available-package name))
             (available-version (when available-pkg
                                  (epl-package-version available-pkg))))
        (when (and available-version (version-list-< version available-version))
          (push (epl-upgrade-create :installed pkg
                                    :available available-pkg)
                upgrades))))
    (nreverse upgrades)))


;;;; Package operations

(defun epl-package-install (package &optional force)
  "Install a PACKAGE.

PACKAGE is a package object.

If FORCE is given and non-nil, install PACKAGE even if it is
already installed."
  (when (or force (not (epl-package-installed-p package)))
    (package-install package)))

(defalias 'epl-package-delete 'package-delete
  "Delete a PACKAGE.

PACKAGE is a package object to delete.")

(provide 'epl-package-desc)

;;; epl-package-desc.el ends here
