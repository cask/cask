(let ((current-directory (file-name-directory load-file-name)))
  (setq carton-test-path (expand-file-name "." current-directory))
  (setq carton-root-path (expand-file-name ".." current-directory)))

(require 'ert)

(load (expand-file-name "carton.el" carton-root-path) t t)
(load (expand-file-name "carton-test.el" carton-test-path) t t)

(ert-run-tests-batch-and-exit t)
