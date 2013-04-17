(require 'elnode)

(defvar port 9191)

(defun archive-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon "(1
     (foo . [(0 0 1) nil \"Foo\" single])
     (bar . [(0 0 2) nil \"Bar\" single]))"))

(defun stop-and-quit ()
  (interactive)
  (elnode-stop port)
  (kill-emacs 0))

(elnode-start 'archive-handler :port port :host "localhost")

(global-set-key (kbd "q") 'stop-and-quit)

(insert
 (format "Running (fake) ELPA server on port %d... Press `q' to quit." port))
