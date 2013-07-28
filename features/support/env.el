(defvar
 carton-features-path
 (file-name-directory
  (directory-file-name (file-name-directory load-file-name))))

(defvar
 carton-vendor-path
 (expand-file-name "vendor" carton-features-path))

(defvar
 carton-root-path
 (expand-file-name ".." (directory-file-name carton-features-path)))

(defvar
 carton-projects-path
 (expand-file-name "projects" carton-features-path))

(defvar
 carton-bin-command
 (expand-file-name "carton" (expand-file-name "bin" carton-root-path)))

(defvar carton-error nil)
(defvar carton-output nil)
(defvar carton-current-project nil)

(add-to-list 'load-path carton-root-path)

(require 's)
(require 'ansi)
(require 'espuds)

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert.el" carton-vendor-path)))

(Before
 (setq carton-error nil)
 (setq carton-output nil)
 (setq carton-current-project nil)

 (mapc
  (lambda (project-path)
    (delete-directory project-path t))
  (directory-files carton-projects-path t "^[^\\.\\.?]")))

(Fail
 (when carton-output
   (princ "==================== CARTON OUTPUT ====================\n")
   (princ carton-output))
 (when carton-error
   (princ "==================== CARTON ERROR ====================\n")
   (princ (ansi-red "%s" carton-error))))
