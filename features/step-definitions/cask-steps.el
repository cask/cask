;;; cask-steps.el --- Cask: Step definitions for Ecukes tests  -*- lexical-binding: t; -*-

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

;; Step definitions for Ecukes integration tests of Cask.

;;; Code:

(defun cask-test/elpa-dir ()
  (f-expand (format ".cask/%s/elpa" emacs-version) cask-current-project))

(defun cask-test/create-project-file (filename content)
  (f-write content 'utf-8 (f-expand filename cask-current-project)))

(defun cask-test/template (command)
  (let* ((command (s-replace "{{EMACS-VERSION}}" emacs-version command))
         (command (s-replace "{{EMACS}}" (getenv "EMACS") command))
         (command (s-replace "{{PROJECTS-PATH}}" cask-projects-path command))
         (command (s-replace "{{PROJECT-PATH}}" cask-current-project command)))
    command))

(Given "^this Cask file:$"
  (lambda (content)
    (cask-test/create-project-file "Cask" content)))

(Given "^I create a file called \"\\([^\"]+\\)\" with content:$"
  (lambda (filename content)
    (cask-test/create-project-file filename content)))

(When "^I run cask \"\\([^\"]*\\)\"$"
  (lambda (command)
    ;; Note: Since the Ecukes tests runs with Casks dependencies in
    ;; EMACSLOADPATH, these will also be available in the subprocess
    ;; created here. Removing all Cask dependencies here to solve it.
    (setenv "EMACSLOADPATH" (s-join path-separator (--reject (s-matches? ".cask" it) load-path)))
    (setq command (cask-test/template command))
    (let* ((buffer-name "*cask-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory
             (if cask-current-project
                 (f-full cask-current-project)
               default-directory))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (exit-code
            (apply
             'call-process
             (append (list cask-bin-command nil buffer nil) args))))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (cond ((= exit-code 0)
                 (setq cask-output content))
                (t
                 (setq cask-error content))))))))

(Given "^I create a project called \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (f-mkdir (f-expand project-name cask-projects-path))))

(When "^I go to the project called \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (setq cask-current-project (f-expand project-name cask-projects-path))))

(Then "^I should see command output:$"
  (lambda (output)
    (should (s-contains? (cask-test/template output) cask-output))))

(Then "^I should see command error:$"
  (lambda (output)
    (should (s-contains? (cask-test/template output) cask-error))))

(Then "^I should not see command output:$"
  (lambda (output)
    (should-not (s-contains? (cask-test/template output) cask-output))))

(Then "^I should not see command error:$"
  (lambda (output)
    (should-not (s-contains? (cask-test/template output) cask-error))))

(Then "^I should see usage information$"
  (lambda ()
    (Then
      "I should see command output:"
      "USAGE: cask [COMMAND] [OPTIONS]")))

(Then "^there should exist a file called \"\\([^\"]+\\)\" with this content:$"
  (lambda (filename content)
    (let ((filepath (f-expand filename cask-current-project)))
      (with-temp-buffer
        (insert-file-contents filepath)
        (Then "I should see:" content)))))

(Then "^there should exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (should (f-dir? (f-expand dirname cask-current-project)))))

(Then "^there should not exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (should-not (f-dir? (f-expand dirname cask-current-project)))))

(Then "^there should exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (should (f-dir? (f-expand dirname (cask-test/elpa-dir))))))

(Then "^there should not exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (should-not (f-dir? (f-expand dirname (cask-test/elpa-dir))))))

(Then "^package directory should not exist$"
  (lambda ()
    (should-not (f-dir? (cask-test/elpa-dir)))))

(When "^I move \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (from to)
    (let ((default-directory cask-current-project))
      (f-move (cask-test/template from) (cask-test/template to)))))

(Then "^I should see cask version$"
  (lambda ()
    (should (s-matches? "^[0-9]+\.[0-9]+\.[0-9]+\n$" cask-output))))

(Then "^I should see no cask file error$"
  (lambda ()
    (should (string= cask-error (concat "Cask file does not exist: \"" (f-expand "Cask" cask-current-project) "\"\n")))))

(provide 'cask-steps)

;;; cask-steps.el ends here
