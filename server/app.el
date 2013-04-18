(require 'elnode)

(defvar port 9191)

(defvar routes
  '(("\/packages\/archive-contents" . archive-handler)
    ("\/packages\/\\(.+\\)-\\(.+\\)\.\\(tar\\|el\\)$" . package-handler)))

(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon routes))

(defun archive-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon "(1
   (foo . [(0 0 1) nil \"Foo\" single])
   (bar . [(0 0 2) nil \"Bar\" single])
   (baz . [(0 0 3) ((qux (0 0 4))) \"Baz\" tar])
   (qux . [(0 0 4) nil \"Qux\" single]))"))

(defun package-handler (httpcon)
  (let* ((name (elnode-http-mapping httpcon 1))
         (version (elnode-http-mapping httpcon 2))
         (format (elnode-http-mapping httpcon 3))
         (content-type
          (if (equal format "el")
              "text/plain"
            "application/x-tar"))
         (filename
          (expand-file-name
           (concat name "-" version "." format) "server")))
    (elnode-http-start httpcon 200 `("Content-type" . ,content-type))
    (elnode-send-file httpcon filename)))

(defun stop-and-quit ()
  (interactive)
  (elnode-stop port)
  (kill-emacs 0))

(global-set-key (kbd "q") 'stop-and-quit)

(insert
 (format "Running (fake) ELPA server on port %d... Press `q' to quit." port))

(elnode-start 'root-handler :port port :host "localhost")
