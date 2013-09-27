;; NOTE:
;; Do not require any helper library such as f, s or dash in
;; here, because then they will pollute the test enviroment by making
;; those libraries available when cask.el run as a test, but not when
;; used as intended.
;;
;; There is another issue since ert-runner is executed with `cask
;; exec`, which means that all Cask dependencies will be in
;; `load-path' here. All of those are removed below to get a clean
;; environment.

(defvar cask-test/test-path
  (file-name-directory load-file-name))

(defvar cask-test/root-path
  (file-name-directory (directory-file-name cask-test/test-path)))

(let (clean-load-path)
  (dolist (path load-path)
    (unless (string-prefix-p cask-test/root-path path)
      (add-to-list 'clean-load-path path)))
  (let ((load-path clean-load-path))
    (require 'cask (expand-file-name "cask.el" cask-test/root-path))))
