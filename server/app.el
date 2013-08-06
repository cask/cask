(require 'elnode)

(defvar port 9191)

(defvar routes
  '(("\/packages\/archive-contents" . archive-handler)
    ("\/new-packages\/archive-contents" . new-archive-handler)
    ("\/\\(new-\\)?packages\/\\(.+\\)-\\(.+\\)\.\\(tar\\|el\\)$" . package-handler)))

(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon routes))

(defun new-archive-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon "(1 (foo . [(0 0 2) nil \"New foo\" single]))"))

(defun archive-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
  (elnode-http-return httpcon "(1
   (foo . [(0 0 1) nil \"Foo\" single])
   (bar . [(0 0 2) nil \"Bar\" single])
   (baz . [(0 0 3) ((qux (0 0 4))) \"Baz\" tar])
   (qux . [(0 0 4) nil \"Qux\" single])
   (hey . [(0 0 5) nil \"Hey\" tar]))"))

(defun package-handler (httpcon)
  (let* ((name (elnode-http-mapping httpcon 2))
         (version (elnode-http-mapping httpcon 3))
         (format (elnode-http-mapping httpcon 4))
         (filename
          (expand-file-name
           (concat name "-" version "." format) "server"))
         (content-type
          (if (equal format "el")
              "application/octet-stream"
            "application/x-tar"))
         (content
          (with-temp-buffer
            (insert-file-contents-literally filename)
            (buffer-string)))
         (content-length (length content)))
    (elnode-http-start
     httpcon 200
     `("Content-type" . ,content-type)
     `("Content-length" . ,content-length))
    (elnode-http-return httpcon content)))

(defun stop-and-quit ()
  (interactive)
  (elnode-stop port)
  (kill-emacs 0))

(global-set-key (kbd "q") 'stop-and-quit)

(insert
 (format "Running (fake) ELPA server on port %d... Press `q' to quit." port))

(elnode-start 'root-handler :port port :host "localhost")

(when noninteractive
  (with-temp-file "tmp/server.pid"
    (insert (format "%s" (emacs-pid))))
  ;; Start "event loop".
  (while t
    ;; We need `while' due to http://stackoverflow.com/questions/14698081/
    (sit-for most-positive-fixnum)))
