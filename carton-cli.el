(require 'carton (expand-file-name "carton.el" (file-name-directory load-file-name)))
(require 'commander)

(defvar dev-mode nil)

(defun carton-cli-setup ()
  (carton-setup default-directory))

(defun carton-cli/package ()
  ""

  )

(defun carton-cli/install ()
  ""

  )

(defun carton-cli/update ()
  ""

  )

(defun carton-cli/exec ()
  ""

  )

(defun carton-cli/init ()
  ""

  )

(defun carton-cli/version ()
  (carton-cli-setup)
  (princ (carton-version)))

(defun carton-cli/name ()
  ""

  )

(defun carton-cli/info ()
  (carton-cli-setup)
  (carton-command-info))

(defun carton-cli/help ()
  ""

  )

(defun carton-cli/dev ()
  (setq dev-mode t))

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
