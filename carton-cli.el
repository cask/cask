;;; carton-cli.el --- Cli interface to Carton

(let ((carton-path (file-name-directory load-file-name)))
  (mapc
   (lambda (package)
     (add-to-list 'load-path package))
   (directory-files (format ".carton/%s/elpa" emacs-version) t "^.+-[^-]+$"))

  (require 'commander)
  (require 'carton (expand-file-name "carton.el" carton-path)))

(defvar carton-cli--dev-mode nil)

(defun carton-cli--setup ()
  (carton-setup default-directory))

(defun carton-cli--print-dependency (dependency)
  (let ((name (carton-dependency-name dependency))
        (version (carton-dependency-version dependency)))
    (princ
     (if version
         (format " - %s (%s)" name version)
       (format " - %s" name)))
    (princ "\n")))

(defun carton-cli/package ()
  (carton-cli--setup)
  (with-temp-file carton-package-file
    (insert (carton-package))))

(defun carton-cli/install ()
  (carton-cli--setup)
  (carton-install))

(defun carton-cli/update ()
  (carton-cli--setup)
  (let ((upgrades (carton-update)))
    (when upgrades
      (princ "Updated packages:\n")
      (dolist (upgrade upgrades)
        (princ
         (format
          "%s %s -> %s\n"
          (carton-upgrade-name upgrade)
          (package-version-join (carton-upgrade-old-version upgrade))
          (package-version-join (carton-upgrade-new-version upgrade))))))))

(defun carton-cli/exec ()
  ""

  )

(defun carton-cli/init ()
  (carton-init default-directory carton-cli--dev-mode))

(defun carton-cli/list ()
  (carton-cli--setup)
  (princ "### Dependencies ###\n\n")
  (princ (format "Runtime [%s]:\n" (length carton-runtime-dependencies)))
  (mapc 'carton-cli--print-dependency carton-runtime-dependencies)
  (if (> (length carton-runtime-dependencies) 0)
      (princ "\n"))
  (princ (format "Development [%s]:\n" (length carton-development-dependencies)))
  (mapc 'carton-cli--print-dependency carton-development-dependencies))

(defun carton-cli/version ()
  (carton-cli--setup)
  (princ (carton-version)))

(defun carton-cli/info ()
  (carton-cli--setup)
  (let* ((info (carton-info))
         (name (carton-package-name info))
         (version (carton-package-version info))
         (description (carton-package-description info)))
    (princ (format "### %s (%s) ###" name version))
    (princ "\n\n")
    (princ description)
    (princ "\n")))

(defun carton-cli/help ()
  (commander-print-usage))

(defun carton-cli/dev ()
  (setq carton-cli--dev-mode t))

(commander
 (name "carton")
 (command "package" "Create -pkg.el file" 'carton-cli/package)
 (command "install" "Install dependencies" 'carton-cli/install)
 (command "update" "Update dependencies" 'carton-cli/update)
 (command "exec" "Execute command with correct dependencies" 'carton-cli/exec)
 (command "init" "Create basic Carton file" 'carton-cli/init)
 (command "version" "Show the package version" 'carton-cli/version)
 (command "list" "List dependencies" 'carton-cli/list)
 (command "info" "Show info about this project" 'carton-cli/info)
 (command "help" "Display this help message" 'carton-cli/help)
 (option "-h, --help" "Display this help message" 'carton-cli/help)
 (option "--dev" "Run in dev mode" 'carton-cli/dev)
 (default "install"))
