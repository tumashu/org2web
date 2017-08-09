;;; org2web-devtools.el --- Functions used to develop org2web

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

;; org2web-config.el contains functions used to develop org2web.

;;; Code:
(require 'org2web)

(defvar org2web-devtools-repository-directory
  "~/project/emacs-packages/org2web/")

(org2web-add-project
 '("org2web"
   :repository-directory (:eval org2web-devtools-repository-directory)
   :remote (git "https://github.com/tumashu/org2web.git" "gh-pages")
   :site-domain "http://tumashu.github.com/org2web"
   :site-main-title "org2web"
   :site-sub-title "(Static site senerator based on org mode)"
   :default-category "documents"
   :theme (worg killjs)
   :force-absolute-url t
   :source-browse-url ("GitHub" "https://github.com/tumashu/org2web")
   :personal-avatar "/media/img/horse.jpg"
   :personal-duoshuo-shortname "tumashu-website"
   :preparation-function org2web-el2org-preparation-function
   :org-export-function org2web-el2org-org-export-function
   :el2org-doc-sources ("org2web.el")
   :el2org-readme-sources ("org2web.el")
   :el2org-index-sources ("org2web.el")
   :web-server-port 6789))

(provide 'org2web-devtools)

;;; org2web-devtools.el ends here
