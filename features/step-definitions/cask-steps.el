(defun cask-test/elpa-dir ()
  (f-expand (format ".cask/%s/elpa" emacs-version) cask-current-project))

(defun cask-test/create-project-file (filename content)
  (f-write (f-expand filename cask-current-project) content))

(defun cask-test/template (command)
  (let* ((command (s-replace "{{EMACS-VERSION}}" emacs-version command))
         (command (s-replace "{{EMACS}}" (getenv "EMACS") command))
         (command (s-replace "{{PROJECT-PATH}}" cask-current-project command)))
    command))

(Given "^this \\(Cask\\|Carton\\) file:$"
  (lambda (filename content)
    (cask-test/create-project-file filename content)))

(Given "^I create a file called \"\\([^\"]+\\)\" with content:$"
  (lambda (filename content)
    (cask-test/create-project-file filename content)))

(When "^I run \\(cask\\|carton\\) \"\\([^\"]*\\)\"$"
  (lambda (binary command)
    (setq command (cask-test/template command))
    (let* ((buffer-name "*cask-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory (file-name-as-directory cask-current-project))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (bin-command
            (if (equal binary "cask")
                cask-bin-command
              carton-bin-command))
           (exit-code
            (apply
             'call-process
             (append (list bin-command nil buffer nil) args))))
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
