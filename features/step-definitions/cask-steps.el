(defun cask-test/elpa-dir ()
  (expand-file-name
   (format ".cask/%s/elpa" emacs-version) cask-current-project))

(defun cask-test/create-project-file (filename content)
  (with-temp-buffer
    (insert content)
    (let ((filepath (expand-file-name filename cask-current-project)))
      (write-file filepath nil))))

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
                (kill-buffer "*cask-output*"))
              (get-buffer-create "*cask-output*")))
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
    (let ((project-path (expand-file-name project-name cask-projects-path)))
      (make-directory project-path))))

(When "^I go to the project called \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (let ((project-path (expand-file-name project-name cask-projects-path)))
      (setq cask-current-project project-path))))

(Then "^I should see command output:$"
  (lambda (output)
    (should (s-contains? (cask-test/template output) cask-output))))

(Then "^I should see command error:$"
  (lambda (output)
    (should (s-contains? (cask-test/template output) cask-error))))

(Then "^I should see usage information$"
  (lambda ()
    (Then
      "I should see command output:"
      "USAGE: cask COMMAND [OPTIONS]")))

(Then "^there should exist a file called \"\\([^\"]+\\)\" with this content:$"
  (lambda (filename content)
    (let ((filepath (expand-file-name filename cask-current-project)))
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (Then "I should see:" content)))))

(Then "^there should exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let ((dirpath (expand-file-name dirname cask-current-project)))
      (should (file-directory-p dirpath)))))

(Then "^there should not exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let ((dirpath (expand-file-name dirname cask-current-project)))
      (should-not (file-directory-p dirpath)))))

(Then "^there should exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let* ((cask-project-path cask-current-project)
           (dirpath (expand-file-name dirname (cask-test/elpa-dir))))
      (should (file-directory-p dirpath)))))

(Then "^there should not exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let* ((cask-project-path cask-current-project)
           (dirpath (expand-file-name dirname (cask-test/elpa-dir))))
      (should-not (file-directory-p dirpath)))))

(Then "^package directory should not exist$"
  (lambda ()
    (let ((cask-project-path cask-current-project))
      (should-not (file-directory-p (cask-test/elpa-dir))))))
