(require 'trace)
(require 'nadvice)
(require 's)

(defvar cask-trace-entry-p nil)
(defvar cask-trace-exit-p nil)

(define-advice trace-entry-message (:around (f &rest args) cask-print)
  (let ((msg (apply f args)))
    (when cask-trace-entry-p
      (message "[trace] %s" (s-trim-right msg)))
    msg))

(define-advice trace-exit-message (:around (f &rest args) cask-print)
  (let ((msg (apply f args)))
    (when cask-trace-exit-p
      (message "[trace] %s" (s-trim-right msg)))
    msg))

;; FIX: These APIs are cleaner, but results in 'Variable binding depth exceeds max-specpdl-size'.
;; (defun cask-print-trace-message (f &rest args)
;;   (let ((msg (apply f args)))
;;     (message (s-trim-right msg))
;;     msg))
;;
;; (defun cask-trace-entry (enabled)
;;   (if enabled
;;       (advice-add 'trace-entry-message :around #'cask-print-trace-message)
;;     (advice-remove 'trace-entry-message #'cask-print-trace-message)))
;;
;; (defun cask-trace-exit (enabled)
;;   (if enabled
;;       (advice-add 'trace-exit-message :around #'cask-print-trace-message)
;;     (advice-remove 'trace-exit-message #'cask-print-trace-message)))

(defun cask-trace-prefix (prefix)
  (mapatoms
   (lambda (s)
     (when (and (string-prefix-p prefix (symbol-name s))
                (functionp s))
       (trace-function s)))))

(defun cask-untrace-prefix (prefix)
  (mapatoms
   (lambda (s)
     (when (and (string-prefix-p prefix (symbol-name s))
                (functionp s))
       (untrace-function s)))))

(provide 'cask-trace)
