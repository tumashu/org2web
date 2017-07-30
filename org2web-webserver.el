;;; org2web-webserver.el --- Test web server required by org2web

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu <tumashu AT 163 DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/tumashu/org2web

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

;; org2web-webserver.el is a web server used to test org2web.

;;; Code:
(require 'cl-lib)
(require 'url-util)
(require 'browse-url)
(require 'simple-httpd)
(require 'org2web-vars)
(require 'org2web-config)

(defvar org2web-webserver-last-docroot nil)
(defvar org2web-webserver-last-port nil)
(defvar org2web-current-project)

(defun org2web-webserver-start (docroot port)
  (org2web-webserver-stop)
  (httpd-log `(start ,(format "org2web: start webserver at %s"
                              (current-time-string))))
  (make-network-process
   :name     (or org2web-current-project "org2web-webserver")
   :service  port
   :server   t
   :host     httpd-host
   :family   httpd-ip-family
   :filter   `(lambda (proc string)
                (let ((httpd-root ,docroot)) ; *override* `http-root' variable
                  (httpd--filter proc string)))
   :filter-multibyte nil
   :coding   'utf-8-unix  ; *should* be ISO-8859-1 but that doesn't work
   :log      'httpd--log))

(defun org2web-webserver-stop ()
  (interactive)
  (let ((name (or org2web-current-project "org2web-webserver")))
    (when (process-status name)
      (delete-process name)
      (httpd-log `(stop ,(format "org2web: stop webserver at %s"
                                 (current-time-string)))))))

(defun org2web-webserver-browse (&optional docroot port)
  (interactive)
  (org2web-webserver-stop)
  (let ((docroot (or docroot org2web-webserver-last-docroot) )
        (port (or port org2web-webserver-last-port)))
    (when (and docroot port)
      (progn
        (org2web-webserver-start docroot port)
        (setq org2web-webserver-last-docroot docroot)
        (setq org2web-webserver-last-port port)
        (browse-url-default-browser
         (format "http://localhost:%s" port))))))

(provide 'org2web-webserver)

;;; org2web-webserver.el ends here
