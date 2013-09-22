;;; cask-bootstrap.el --- Bootstrap Casks's internal dependencies

(eval-when-compile
  (defvar cask-cli-directory))

(defconst cask-bootstrap-dir
  (expand-file-name
   (locate-user-emacs-file (format ".cask/%s/bootstrap" emacs-version)))
  "Path to Cask bootstrap directory.")

(defconst cask-bootstrap-packages '(commander git f s dash epl)
  "List of bootstrap packages required by this file.")

(unless (require 'package nil :noerror)
  (require 'package (expand-file-name "package-legacy" cask-cli-directory)))

(let ((orig-load-path load-path))
  (unwind-protect
      (let (package-archives
            package-alist
            package-archive-contents
            (package-user-dir cask-bootstrap-dir))
        (package-initialize)
        (condition-case nil
            (mapc 'require cask-bootstrap-packages)
          (error
           (add-to-list 'package-archives (cons "gnu" "http://elpa.gnu.org/packages/"))
           (add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
           (package-refresh-contents)
           (mapc
            (lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
            cask-bootstrap-packages)
           (mapc 'require cask-bootstrap-packages))))
    (setq load-path orig-load-path)))

(provide 'cask-bootstrap)

;;; cask-bootstrap.el ends here
