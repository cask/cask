;;; cask-trace.el --- Cask: Tracing function calls  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Johan Andersson

;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
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

;; Cask's tracing support, for troubleshooting non-interactive environments.

;;; Code:

(require 'trace)
(require 'nadvice)
(require 's)

(defun cask-trace-print-message (f &rest args)
  (let ((msg (apply f args)))
    (message "[trace] %s" (s-trim-right msg))
    msg))

(defun cask-trace-print-entry (enabled)
  (if enabled
      (advice-add 'trace-entry-message :around #'cask-trace-print-message)
    (advice-remove 'trace-entry-message #'cask-trace-print-message)))

(defun cask-trace-print-exit (enabled)
  (if enabled
      (advice-add 'trace-exit-message :around #'cask-trace-print-message)
    (advice-remove 'trace-exit-message #'cask-trace-print-message)))

(defun cask-trace--map-prefix (prefix function)
  (mapatoms
   (lambda (s)
     (when (and (string-prefix-p prefix (symbol-name s))
                (functionp s)
                (not (eq s #'cask-trace-print-message)))
       (funcall function s)))))

(defun cask-trace-prefix (prefix)
  (cask-trace--map-prefix prefix #'trace-function))

(defun cask-untrace-prefix (prefix)
  (cask-trace--map-prefix prefix #'untrace-function))

(provide 'cask-trace)
;;; cask-trace.el ends here
