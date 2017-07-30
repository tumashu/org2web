;;; owp-webserver.el --- Test web server required by org-webpage

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

;; owp-webserver.el is a web server used to test org-webpage.

;;; Code:
(require 'cl-lib)
(require 'url-util)
(require 'browse-url)
(require 'simple-httpd)
(require 'owp-vars)
(require 'owp-config)

(defvar owp-webserver-last-docroot nil)
(defvar owp-webserver-last-port nil)
(defvar owp-current-project-name)

(defun owp-webserver-start (docroot port)
  (owp-webserver-stop)
  (httpd-log `(start ,(format "org-webpage: start webserver at %s"
                              (current-time-string))))
  (make-network-process
   :name     (or owp-current-project-name "owp-webserver")
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

(defun owp-webserver-stop ()
  (interactive)
  (let ((name (or owp-current-project-name "owp-webserver")))
    (when (process-status name)
      (delete-process name)
      (httpd-log `(stop ,(format "org-webpage: stop webserver at %s"
                                 (current-time-string)))))))

(defun owp-webserver-browse (&optional docroot port)
  (interactive)
  (owp-webserver-stop)
  (let ((docroot (or docroot owp-webserver-last-docroot) )
        (port (or port owp-webserver-last-port)))
    (when (and docroot port)
      (progn
        (owp-webserver-start docroot port)
        (setq owp-webserver-last-docroot docroot)
        (setq owp-webserver-last-port port)
        (browse-url-default-browser
         (format "http://localhost:%s" port))))))

(provide 'owp-webserver)

;;; owp-webserver.el ends here
