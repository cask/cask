(require 'f)

(defvar cask-test/test-path
  (f-parent (f-this-file)))

(defvar cask-test/root-path
  (f-parent cask-test/test-path))

(defvar cask-test/sandbox-path
  (f-expand "sandbox" cask-test/test-path))

(defvar cask-test/package-path
  (f-expand "package" cask-test/sandbox-path))

(defvar cask-test/config-path
  (f-expand "config" cask-test/sandbox-path))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory cask-test/sandbox-path))
     ,@body))

;; Do not pollute the Cask environment.
(unload-feature 'f 'force)

;; Since ert-runner is executed with `cask exec` all Cask dependencies
;; will be in `load-path'. This cleans up the environment.
(let (clean-load-path)
  (dolist (path load-path)
    (unless (string-prefix-p cask-test/root-path path)
      (add-to-list 'clean-load-path path)))
  (let ((load-path clean-load-path))
    (require 'cask (expand-file-name "cask.el" cask-test/root-path))))
