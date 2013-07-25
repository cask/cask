;;; epl.el --- Emacs Package Library -*- lexical-binding: t; -*-

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

;; A package management library for Emacs, based on package.el.

;; The purpose of this library is to wrap all the quirks and hassle of
;; package.el into a sane API.

;; The following functions comprise the public interface of this library:

;; `epl-package-p' determines whether an object is a package object.

;; `epl-package-name' gets the package name from a package object.

;; `epl-package-summary' gets the package summary from a package object.

;; `epl-package-version' gets the package version from a package object.

;; `epl-package-version-string' gets the package version as string from a
;; package object.

;; `epl-package-requirements' gets the requirements of a package.

;; `epl-package-installed-p' determines whether a package is installed.

;; `epl-initialize' initializes the package system and activates all
;; packages.

;; `epl-reset' resets the package system.

;; `epl-refresh' refreshes all package archives.

;; `epl-package-dir' gets the directory of packages.

;; `epl-default-package-dir' gets the default package directory.

;; `epl-change-package-dir' changes the directory of packages.

;; `epl-add-archive' adds a new package archive.

;; `epl-installed-packages' and `epl-available-packages' get all packages
;; installed and available for installed respectively.

;; `epl-find-installed-package' and `epl-find-available-package' find installed
;; and available packages by name.

;; `epl-find-upgrades' finds all upgradable packages.

;; `epl-install' installs a package.

;; `epl-delete' deletes a package.

;; `epl-upgrade' upgrades packages.

;; This version of the EPL library requires a recent package.el which provides
;; the `package-desc' struct.  Loading it with a legacy package.el signals an
;; error.

;;; Code:


;;;; Load package.el

(require 'package)

(setq package-archives nil)             ; Clear the default list of archives to
                                        ; let the user have exact control over
                                        ; all archives


;;;; Compatibility check

;; Declare our error symbol
(put 'epl-error 'error-conditions '(error))

(unless (fboundp 'package-desc-create)
  ;; The package-desc structure is missing, hence indicate that this API cannot
  ;; be loaded
  (signal 'epl-error "Legacy package.el API detected"))


;;;; Other requirements

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

(defun epl-requirement-version-string (requirement)
  "The version of a REQUIREMENT, as string."
  (package-version-join (epl-requirement-version requirement)))

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

(defun epl-package-version-string (package)
  "Get the version from a PACKAGE, as string."
  (package-version-join (epl-package-version package)))

(defun epl-package-dir ()
  "Get the directory of packages."
  package-user-dir)

(defun epl-default-package-dir ()
  "Get the default directory of packages."
  (eval (car (get 'package-user-dir 'standard-value))))

(defun epl-change-package-dir (directory)
  "Change the directory of packages to DIRECTORY."
  (setq package-user-dir directory)
  (epl-initialize))

(defun epl-package-installed-p (package)
  "Determine whether a PACKAGE is installed.

PACKAGE is either a package name as symbol, or a package object."
  (let ((name (if (epl-package-p package)
                  (epl-package-name package)
                package))
        (version (when (epl-package-p package)
                   (epl-package-version package))))
    (package-installed-p name version)))

(defun epl-package-from-buffer (&optional buffer)
  "Create a `epl-package' object from a BUFFER.

Parse the package metadata of BUFFER and return a corresponding
`epl-package' object."
  (with-current-buffer buffer
    (package-buffer-info)))


;;;; Package system management

(defalias 'epl-initialize 'package-initialize)

(defun epl-reset ()
  "Reset the package system.

Clear the list of installed and available packages, the list of
package archives and reset the package directory."
  (setq package-alist nil
        package-archives nil
        package-archive-contents nil)
  (epl-change-package-dir (epl-default-package-dir)))

(defalias 'epl-refresh 'package-refresh-contents)

(defun epl-add-archive (name url)
  "Add a package archive with NAME and URL."
  (add-to-list 'package-archives (cons name url)))


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

(defun epl-install (package &optional force)
  "Install a PACKAGE.

PACKAGE is a package object.

If FORCE is given and non-nil, install PACKAGE even if it is
already installed."
  (when (or force (not (epl-package-installed-p package)))
    (package-install package)))

(defalias 'epl-delete 'package-delete
  "Delete a PACKAGE.

PACKAGE is a package object to delete.")

(defun epl-upgrade (&optional packages preserve-obsolete)
  "Upgrade PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

The old versions of the updated packages are deleted, unless
PRESERVE-OBSOLETE is non-nil.

Return a list of all performed upgrades, as a list of
`epl-upgrade' objects."
  (let ((upgrades (epl-find-upgrades packages)))
    (dolist (upgrade upgrades)
      (epl-install (epl-upgrade-available upgrade))
      (unless preserve-obsolete
        (epl-delete (epl-upgrade-installed upgrade))))))

(provide 'epl)

;;; epl.el ends here
