;;; app.el --- Cask: Stub ELPA server for tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/cask/cask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple stub ELPA server for Cask's tests.

;;; Code:

(require 'f)
(require 'servant)

(let* ((port 9191)
       (host "127.0.0.1")
       (directory (f-parent (f-this-file)))
       (tmp-directory (f-join directory "tmp"))
       (routes (--map
                (cons (format "^.*//%s/\\(.*\\)$" (regexp-quote it))
                      (servant-make-elnode-handler (f-join directory it)))
                '("packages" "new-packages"))))

  (elnode-start (lambda (httpcon) (elnode-hostpath-dispatcher httpcon routes))
                :port port :host host)

  (when noninteractive
    (f-mkdir tmp-directory)
    (with-temp-file (f-join tmp-directory "servant.pid")
      (insert (format "%s" (emacs-pid))))
    ;; Start "event loop".
    (while t
      ;; We need `while' due to http://stackoverflow.com/questions/14698081/
      (sit-for 10))))

;;; app.el ends here
