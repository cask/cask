(setq
 carton-features-path
 (file-name-directory
  (directory-file-name (file-name-directory load-file-name))))

(setq
 carton-root-path
 (expand-file-name ".." (directory-file-name carton-features-path)))

(setq
 carton-projects-path
 (expand-file-name "projects" carton-features-path))

(setq
 carton-bin-command
 (expand-file-name "carton" (expand-file-name "bin" carton-root-path)))

(add-to-list 'load-path carton-root-path)

(require 's)
(require 'carton)
(require 'espuds)
(require 'ert)

(Before
 (mapc
  (lambda (project-path)
    (delete-directory project-path t nil))
  (directory-files carton-projects-path t "^[^\\..\\?]")))
