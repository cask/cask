;;; env.el --- Cask: Ecukes environment file  -*- lexical-binding: t; -*-

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

;;; Code:

(require 's)
(require 'f)
(require 'espuds)
(require 'ansi)

(defvar cask-test/features-path
  (f-parent (f-parent load-file-name)))

(defvar cask-test/vendor-path
  (f-expand "vendor" cask-test/features-path))

(defvar cask-test/root-path
  (f-parent cask-test/features-path))

(defvar cask-test/sandbox-path
  (f-expand "sandbox" cask-test/features-path))

(defvar cask-bin-path
  (f-expand "bin" cask-test/root-path))

(defvar cask-test/bin-command
  (f-expand "cask" cask-bin-path))

(defvar cask-test/stderr)
(defvar cask-test/stdout)

(defvar cask-initial-$PATH)
(defvar cask-initial-$TRAVIS)

(add-to-list 'load-path cask-test/root-path)

(unless (require 'ert nil t)
  (require 'ert (f-expand "ert" cask-test/vendor-path)))

(Setup
 (setq cask-initial-$PATH (getenv "PATH"))
 (setq cask-initial-$TRAVIS (getenv "TRAVIS")))

(Before
 (setq cask-test/stderr "")
 (setq cask-test/stdout "")

 (when (f-dir? cask-test/sandbox-path)
   (f-delete cask-test/sandbox-path 'force))
 (f-mkdir cask-test/sandbox-path)

 (setenv "PATH" cask-initial-$PATH)
 (setenv "TRAVIS" cask-initial-$TRAVIS))

(Fail
 (-when-let (stdout (s-presence cask-test/stdout))
   (princ "==================== CASK OUTPUT ====================\n")
   (princ stdout))
 (-when-let (stderr (s-presence cask-test/stderr))
   (princ "==================== CASK OUTPUT ====================\n")
   (princ (ansi-red "%s" stderr))))

;;; env.el ends here
