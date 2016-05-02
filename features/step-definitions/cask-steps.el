;;; cask-steps.el --- Cask: Step definitions for Ecukes tests  -*- lexical-binding: t; -*-

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

;; Step definitions for Ecukes integration tests of Cask.


;;; Code:

(eval-when-compile
  (defvar cask-test/sandbox-path)
  (defvar cask-test/bin-command)
  (defvar cask-test/stdout)
  (defvar cask-test/stderr))

(require 'shell-split-string)

(defun cask-test/template (command)
  "Return COMMAND with placeholders replaced with values."
  (->> command
    (s-replace "{{EMACS-VERSION}}" emacs-version)
    (s-replace "{{EMACS}}" (executable-find (or (getenv "EMACS") "emacs")))))

(Given "^this Cask file:$"
  (lambda (content)
    (f-write-text content 'utf-8 (f-expand "Cask" cask-test/sandbox-path))))

(Given "^I create a file called \"\\([^\"]+\\)\" with content:$"
  (lambda (filename content)
    (f-write-text content 'utf-8 (f-expand filename cask-test/sandbox-path))))

(When "^I run cask \"\\(.*\\)\"$"
  (lambda (command)
    ;; Note: Since the Ecukes tests runs with Casks dependencies in
    ;; EMACSLOADPATH, these will also be available in the subprocess
    ;; created here. Removing all Cask dependencies here to solve it.
    (setenv "EMACSLOADPATH" (s-join path-separator (--reject (s-matches? ".cask" it) load-path)))
    (with-temp-buffer
      (let* ((default-directory (f-full cask-test/sandbox-path))
             (args (shell-split-string (cask-test/template command)))
             (exit-code
              (apply
               'call-process
               (append (list cask-test/bin-command nil (current-buffer) nil) args))))
        (let ((content (buffer-string)))
          (cond ((= exit-code 0)
                 (setq cask-test/stdout content))
                (t
                 (setq cask-test/stderr content))))))))

(Then "^I should see command error:$"
  (lambda (output)
    (should (s-contains? output cask-test/stderr))))

(Then "^I should see command output:$"
  (lambda (output)
    (should (s-contains? output cask-test/stdout))))

(Then "^I should not see command error:$"
  (lambda (output)
    (should-not (s-contains? output cask-test/stderr))))

(Then "^I should not see command output:$"
  (lambda (output)
    (should-not (s-contains? output cask-test/stdout))))

(Then "^I should see no command output$"
  (lambda ()
    (should (s-blank? cask-test/stdout))))

(Then "^I should see usage information$"
  (lambda ()
    (should (s-contains? "USAGE: cask [COMMAND] [OPTIONS]" cask-test/stdout))))

(Then "^package \"\\([^\"]+\\)\" should be installed$"
  (lambda (package)
    (should (f-dir? (f-join cask-test/sandbox-path ".cask" emacs-version "elpa" package)))))

(Then "^package \"\\([^\"]+\\)\" should not be installed$"
  (lambda (package)
    (should-not (f-dir? (f-join cask-test/sandbox-path ".cask" emacs-version "elpa" package)))))

(provide 'cask-steps)

;;; cask-steps.el ends here
