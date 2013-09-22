(require 'f)

(defvar cask-test/test-path
  (f-dirname (f-this-file)))

(defvar cask-test/root-path
  (f-parent cask-test/test-path))

(require 'cask (f-expand "cask.el" cask-test/root-path))
