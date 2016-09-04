;;; owp-web-server.el --- Test web server required by org-webpage

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu <tumashu AT 163 DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/tumashu/org-webpage

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

;; owp-web-server.el is a web server used to test org-webpage.

;;; Code:
(require 'cl-lib)
(require 'url-util)
(require 'browse-url)
(require 'simple-httpd)
(require 'owp-vars)
(require 'owp-config)

(defvar owp/last-web-server-docroot nil)
(defvar owp/last-web-server-port nil)
(defvar owp/current-project-name)

(defun owp/web-server-start (docroot port)
  (owp/web-server-stop)
  (httpd-log `(start ,(format "org-webpage: start web-server at %s"
                              (current-time-string))))
  (make-network-process
   :name     (or owp/current-project-name "owp-httpd")
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

(defun owp/web-server-stop ()
  (interactive)
  (let ((name (or owp/current-project-name "owp-httpd")))
    (when (process-status name)
      (delete-process name)
      (httpd-log `(stop ,(format "org-webpage: stop web-server at %s"
                                 (current-time-string)))))))

(defun owp/web-server-browse (&optional docroot port)
  (interactive)
  (owp/web-server-stop)
  (let ((docroot (or docroot owp/last-web-server-docroot) )
        (port (or port owp/last-web-server-port)))
    (when (and docroot port)
      (progn
        (owp/web-server-start docroot port)
        (setq owp/last-web-server-docroot docroot)
        (setq owp/last-web-server-port port)
        (browse-url-default-browser
         (format "http://localhost:%s" port))))))

(provide 'owp-web-server)

;;; owp-web-server.el ends here
