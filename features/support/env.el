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

(defvar cask-projects-path
  (f-expand "projects" cask-features-path))

(defvar cask-bin-path
  (f-expand "bin" cask-root-path))

(defvar cask-bin-command
  (f-expand "cask" cask-bin-path))

(defvar carton-bin-command
  (f-expand "carton" cask-bin-path))

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
  (f-directories cask-projects-path)))

(Fail
 (unless (s-blank? cask-output)
   (princ "==================== CASK OUTPUT ====================\n")
   (princ cask-output))
 (unless (s-blank? cask-error)
   (princ "==================== CASK ERROR ====================\n")
   (princ (ansi-red "%s" cask-error))))
