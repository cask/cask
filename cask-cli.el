;;; cask-cli.el --- Cli interface to Cask

;; Avoid "Loading vc-git..." messages
(remove-hook 'find-file-hooks 'vc-find-file-hook)


(eval-and-compile
  (defconst cask-cli-directory
    (file-name-directory
     (cond
      (load-in-progress load-file-name)
      ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
       byte-compile-current-file)
      (:else (buffer-file-name))))
    "Path to Cask root."))

(require 'cask-bootstrap (expand-file-name "cask-bootstrap" cask-cli-directory))
(require 'cask (expand-file-name "cask" cask-cli-directory))

(defvar cask-cli--dev-mode nil
  "If Cask should run in dev mode or not.")

(defun cask-cli--find-unbalanced-parenthesis ()
  (with-temp-buffer
    (insert (f-read-text cask-file 'utf-8))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (check-parens)
          nil)
      (error (cask-current-source-position)))))

(defun cask-cli--exit-error (err)
  (let ((type (car err))
        (data (cdr err))
        pos msg)
    (if (eq type 'end-of-file)
        ;; In case of premature end of file, try hard to find the real
        ;; position, by scanning for unbalanced parenthesis
        (setq pos (or (cask-cli--find-unbalanced-parenthesis) (cadr err))
              msg "End of file while reading (possible unbalanced parenthesis)")
      ;; For other types of error, check whether the error has a position, and
      ;; print it.  Otherwise just print the error like Emacs would do
      (when (cask-source-position-p (car data))
        (setq pos (car data))
        ;; Strip the position from the error data
        (setq data (cdr data)))
      (setq msg (error-message-string (cons type data))))
    (if pos
        (message "%s:%s:%s: %s" cask-file (cask-source-position-line pos)
                 (cask-source-position-column pos) msg)
      (message "%s: %s" cask-file msg)))
  (kill-emacs 1))

(defun cask-cli--setup ()
  (condition-case err
      (cask-setup default-directory)
    (end-of-file
     (cask-cli--exit-error err))
    (invalid-read-syntax
     (cask-cli--exit-error err))))

(defun cask-cli--print-dependency (dependency)
  (let ((name (cask-dependency-name dependency))
        (version (cask-dependency-version dependency)))
    (princ
     (if version
         (format " - %s (%s)" name version)
       (format " - %s" name)))
    (princ "\n")))

(defun cask-cli--print-upgrade (upgrade)
  (princ
   (format
    "%s %s -> %s\n"
    (epl-package-name (epl-upgrade-installed upgrade))
    (epl-package-version-string (epl-upgrade-installed upgrade))
    (epl-package-version-string (epl-upgrade-available upgrade)))))

(defun cask-cli/package ()
  (cask-cli--setup)
  (f-write-text (cask-package) 'utf-8 cask-package-file))

(defun cask-cli/install ()
  (cask-cli--setup)
  (cask-install))

(defun cask-cli/upgrade ()
  (unwind-protect
      (progn
        (epl-change-package-dir cask-bootstrap-dir)
        (epl-initialize)
        (epl-add-archive "gnu" "http://elpa.gnu.org/packages/")
        (epl-add-archive "melpa" "http://melpa.milkbox.net/packages/")
        (epl-refresh)
        (epl-upgrade))
    (epl-reset))
  (require 'git)
  (let ((git-repo cask-cli-directory))
    (if (s-present? (git-run "status" "--porcelain"))
        (error "Cannot update Cask because of dirty tree")
      (git-pull))))

(defun cask-cli/update ()
  (cask-cli--setup)
  (-when-let (upgrades (cask-update))
    (princ "Updated packages:\n")
    (-each upgrades 'cask-cli--print-upgrade)))

(defun cask-cli/init ()
  (cask-init default-directory cask-cli--dev-mode))

(defun cask-cli/list ()
  (cask-cli--setup)
  (princ "### Dependencies ###\n\n")
  (princ (format "Runtime [%s]:\n" (length cask-runtime-dependencies)))
  (mapc 'cask-cli--print-dependency cask-runtime-dependencies)
  (if (> (length cask-runtime-dependencies) 0)
      (princ "\n"))
  (princ (format "Development [%s]:\n" (length cask-development-dependencies)))
  (mapc 'cask-cli--print-dependency cask-development-dependencies))

(defun cask-cli/version ()
  (cask-cli--setup)
  (princ (cask-version)))

(defun cask-cli/info ()
  (cask-cli--setup)
  (let* ((info (cask-info))
         (name (cask-package-name info))
         (version (cask-package-version info))
         (description (cask-package-description info)))
    (princ (format "### %s (%s) ###" name version))
    (princ "\n\n")
    (princ description)
    (princ "\n")))

(defun cask-cli/help ()
  (commander-print-usage-and-exit))

(defun cask-cli/load-path ()
  (princ (cask-load-path)))

(defun cask-cli/path ()
  (princ (cask-path)))

(defun cask-cli/package-directory ()
  (princ (cask-elpa-dir)))

(defun cask-cli/dev ()
  (setq cask-cli--dev-mode t))

(defun cask-cli/debug ()
  (setq debug-on-error t)
  (setq debug-on-entry t))

(defun cask-cli/outdated ()
  (cask-cli--setup)
  (-when-let (outdated (cask-outdated))
    (princ "Outdated packages:\n")
    (-each outdated 'cask-cli--print-upgrade)))

(commander
 (name "cask")
 (description "Emacs dependency management made easy")

 (default "install")

 (command "package" "Create -pkg.el file" cask-cli/package)
 (command "install" "Install dependencies" cask-cli/install)
 (command "update" "Update dependencies" cask-cli/update)
 (command "upgrade" "Upgrade Cask" cask-cli/upgrade)
 (command "exec [*]" "Execute command with correct dependencies" ignore)
 (command "init" "Create basic Cask file" cask-cli/init)
 (command "version" "Show the package version" cask-cli/version)
 (command "list" "List dependencies" cask-cli/list)
 (command "info" "Show info about this project" cask-cli/info)
 (command "help" "Display this help message" cask-cli/help)
 (command "load-path" "Print Emacs load-path (including package dependencies)" cask-cli/load-path)
 (command "path" "Print Emacs exec-path (including package bin path)" cask-cli/path)
 (command "package-directory" "Print package installation directory" cask-cli/package-directory)
 (command "outdated" "Show list of outdated packages" cask-cli/outdated)

 (option "-h, --help" "Display this help message" cask-cli/help)
 (option "--dev" "Run in dev mode" cask-cli/dev)
 (option "--debug" "Turn on debug output" cask-cli/debug))

;;; cask-cli.el ends here
