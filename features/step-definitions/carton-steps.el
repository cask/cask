(defun carton-test/elpa-dir ()
  (expand-file-name
   (format ".carton/%s/elpa" emacs-version) carton-current-project))

(defun carton-test/create-project-file (filename content)
  (with-temp-buffer
    (insert content)
    (let ((filepath (expand-file-name filename carton-current-project)))
      (write-file filepath nil))))

(Given "^this Carton file:$"
  (lambda (content)
    (carton-test/create-project-file "Carton" content)))

(Given "^I create a file called \"\\([^\"]+\\)\" with content:$"
  (lambda (filename content)
    (carton-test/create-project-file filename content)))

(When "^I run carton \"\\([^\"]*\\)\"$"
  (lambda (command)
    (setq command (s-replace "{{EMACS-VERSION}}" emacs-version command))
    (setq command (s-replace "{{EMACS}}" (getenv "EMACS") command))
    (let* ((buffer (get-buffer-create "*carton-output*"))
           (default-directory (file-name-as-directory carton-current-project))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (exit-code
            (apply
             'call-process
             (append (list carton-bin-command nil buffer nil) args))))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (cond ((= exit-code 0)
                 (setq carton-output content))
                (t
                 (setq carton-error content))))))))

(Given "^I create a project called \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (let ((project-path (expand-file-name project-name carton-projects-path)))
      (make-directory project-path))))

(When "^I go to the project called \"\\([^\"]+\\)\"$"
  (lambda (project-name)
    (let ((project-path (expand-file-name project-name carton-projects-path)))
      (setq carton-current-project project-path))))

(Then "^I should see command output:$"
  (lambda (output)
    (should (s-contains? output carton-output))))

(Then "^I should see command error:$"
  (lambda (output)
    (should (s-contains? output carton-error))))

(Then "^I should see usage information$"
  (lambda ()
    (Then
      "I should see command output:"
      "USAGE: carton COMMAND [OPTIONS]")))

(Then "^there should exist a file called \"\\([^\"]+\\)\" with this content:$"
  (lambda (filename content)
    (let ((filepath (expand-file-name filename carton-current-project)))
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (Then "I should see:" content)))))

(Then "^there should exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let ((dirpath (expand-file-name dirname carton-current-project)))
      (should (file-directory-p dirpath)))))

(Then "^there should not exist a directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let ((dirpath (expand-file-name dirname carton-current-project)))
      (should-not (file-directory-p dirpath)))))

(Then "^there should exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let* ((carton-project-path carton-current-project)
           (dirpath (expand-file-name dirname (carton-test/elpa-dir))))
      (should (file-directory-p dirpath)))))

(Then "^there should not exist a package directory called \"\\([^\"]+\\)\"$"
  (lambda (dirname)
    (let* ((carton-project-path carton-current-project)
           (dirpath (expand-file-name dirname (carton-test/elpa-dir))))
      (should-not (file-directory-p dirpath)))))

(Then "^package directory should not exist$"
  (lambda ()
    (let ((carton-project-path carton-current-project))
      (should-not (file-directory-p (carton-test/elpa-dir))))))
