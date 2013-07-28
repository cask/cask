(require 's)
(require 'f)
(require 'dash)
(require 'espuds)
(require 'ansi)

(defvar carton-features-path
  (f-parent (f-parent load-file-name)))

(defvar carton-vendor-path
  (f-expand "vendor" carton-features-path))

(defvar carton-root-path
  (f-parent carton-features-path))

(defvar carton-projects-path
  (f-expand "projects" carton-features-path))

(defvar carton-bin-command
  (f-expand (f-join "bin" "carton") carton-root-path))

(defvar carton-error nil)
(defvar carton-output nil)
(defvar carton-current-project nil)

(add-to-list 'load-path carton-root-path)

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" carton-vendor-path)))

(Before
 (setq carton-error nil)
 (setq carton-output nil)
 (setq carton-current-project nil)

 (--map
  (f-delete it t)
  (f-directories carton-projects-path)))

(Fail
 (when carton-output
   (princ "==================== CARTON OUTPUT ====================\n")
   (princ carton-output))
 (when carton-error
   (princ "==================== CARTON ERROR ====================\n")
   (princ (ansi-red "%s" carton-error))))
