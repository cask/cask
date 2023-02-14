;;; cask-cli-test.el --- Cask: Cli tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Johan Andersson

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

;; This file contains tests for the Cask CLI.

;;; Code:

(defmacro cask-cli-test/annulled (commands &rest body)
  (declare (indent defun))
  `(cl-letf ,(mapcar (lambda (x)
                       `((symbol-function ',(intern (concat "commander-" (symbol-name x))))
                         (function ignore)))
                     commands)
     ,@body))

(cask-cli-test/annulled (option command parse name description config default)
  (require 'cask-cli))

(ert-deftest cask-cli-test/print-table-no-links ()
  (cask-test/with-bundle 'empty
    (cl-letf (((symbol-function 'princ) #'insert))
      (with-temp-buffer
        (cask-cli--print-table (cask-links bundle))
        (should (zerop (length (buffer-string))))))))

(ert-deftest cask-cli-test/failed-installation ()
  (cask-test/with-bundle 'empty
    (cl-letf (((symbol-function 'epl-upgrade)
               (apply-partially #'signal 'cask-failed-installation '("foo" "bar"))))
      (let (debug-on-error)
        (condition-case err
            (cask-cli/with-handled-errors
             (cask-update bundle)
             ;; should err out before this point
             (should nil))
          (error (should (string-match-p "^Package installation failed"
                                         (car (cdr err))))))))))

(ert-deftest cask-cli-test/print-table-with-links ()
  (cask-test/with-bundle
      '((source localhost)
        (depends-on "package-c" "0.0.1")
        (depends-on "package-d" "0.0.1"))
    (let ((package-c-path (cask-test/link bundle 'package-c "package-c-0.0.1"))
          (package-d-path (cask-test/link bundle 'package-d "package-d-0.0.1")))
      (cl-letf (((symbol-function 'princ) #'insert))
        (with-temp-buffer
          (cask-cli--print-table (cask-links bundle))
          (should-not (zerop (length (buffer-string)))))))))
