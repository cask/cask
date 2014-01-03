(require 's)
(require 'f)
(require 'dash)
(require 'espuds)
(require 'ansi)

(defvar cask-features-path
  (f-parent (f-parent load-file-name)))

(defvar cask-vendor-path
  (f-expand "vendor" cask-features-path))

(defvar cask-root-path
  (f-parent cask-features-path))

(defvar cask-sandbox-path
  (f-expand "sandbox" cask-features-path))

(defvar cask-bin-path
  (f-expand "bin" cask-root-path))

(defvar cask-bin-command
  (f-expand "cask" cask-bin-path))

(defvar cask-link-foo-path
  (f-expand "link-foo" cask-sandbox-path))

(defvar cask-link-new-foo-path
  (f-expand "link-new-foo" cask-sandbox-path))

(defvar cask-link-bar-path
  (f-expand "link-bar" cask-sandbox-path))

(defvar cask-error nil)
(defvar cask-output nil)
(defvar cask-current-project nil)

(add-to-list 'load-path cask-root-path)

(unless (require 'ert nil t)
  (require 'ert (f-expand "ert" cask-vendor-path)))

(Before
 (setq cask-error "")
 (setq cask-output "")
 (setq cask-current-project nil)

 (--map
  (f-delete it t)
  (f-directories cask-sandbox-path))

 (f-mkdir cask-link-foo-path)
 (f-mkdir cask-link-new-foo-path)
 (f-mkdir cask-link-bar-path))

(Fail
 (unless (s-blank? cask-output)
   (princ "==================== CASK OUTPUT ====================\n")
   (princ cask-output))
 (unless (s-blank? cask-error)
   (princ "==================== CASK ERROR ====================\n")
   (princ (ansi-red "%s" cask-error))))
