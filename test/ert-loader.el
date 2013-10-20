(require 'f)

(defvar cask-test/test-path
  (f-dirname (f-this-file)))

(defvar cask-test/root-path
  (f-parent cask-test/test-path))

(defvar cask-test/vendor-path
  (f-expand "vendor" cask-test/test-path))

(unless (require 'ert nil t)
  (require 'ert (f-expand "ert" cask-test/vendor-path)))
