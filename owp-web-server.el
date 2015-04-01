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
(require 'url-util)
(require 'web-server)
(require 'owp-vars)
(require 'owp-config)

(defvar owp/web-server nil)

(defun owp/web-server-get-url ()
  (file-name-as-directory
   (format "http://localhost:%s"
           (number-to-string
            (owp/get-config-option :web-server-port)))))

(defun owp/web-server-start ()
  (interactive)
  (lexical-let ((docroot
                 (expand-file-name
                  (owp/get-config-option :web-server-docroot)))
                (port (owp/get-config-option :web-server-port)))
    (when (and (not owp/web-server)
               docroot port)
      (setq owp/web-server
            (ws-start
             (lambda (request)
               (with-slots (process headers) request
                 (let* ((path (substring (decode-coding-string
                                          (url-unhex-string (cdr (assoc :GET headers)))
                                          'utf-8) 1))
                        (path-expand (expand-file-name path docroot))
                        (path-index-file (concat (file-name-as-directory path-expand)
                                                 "index.html")))
                   (if (or (ws-in-directory-p docroot path-expand)
                           (< (length path) 1))
                       (cond
                        ((file-exists-p path-index-file)
                         (ws-send-file process path-index-file))
                        ((and (file-exists-p path-expand)
                              (not (file-directory-p path-expand)))
                         (ws-send-file process path-expand))
                        ((file-directory-p path-expand)
                         (ws-send-directory-list process path-expand))
                        (t (ws-send-404 process)))
                     (ws-send-404 process)))))
             (owp/get-config-option :web-server-port))))))

(defun owp/web-server-stop ()
  (interactive)
  (when owp/web-server
    (ws-stop owp/web-server)
    (setq owp/web-server nil)))

(defun owp/web-server-browse ()
  (interactive)
  (owp/web-server-stop)
  (owp/web-server-start)
  (when owp/web-server
    (browse-url-default-browser
     (owp/web-server-get-url))))

(provide 'owp-web-server)

;;; owp-web-server.el ends here
