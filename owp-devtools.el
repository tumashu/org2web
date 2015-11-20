;;; owp-devtools.el --- Functions used to develop org-webpage

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

;; owp-config.el contains functions used to develop org-webpage.

;;; Code:
(require 'org)
(require 'ox-org)
(require 'owp-vars)
(require 'owp-config)
(require 'owp-lentic)

(defvar owp/devtools-repository-directory
  "~/project/emacs-packages/org-webpage/")

(owp/add-project-config
 '("org-webpage"
   :repository-directory (:eval owp/devtools-repository-directory)
   :remote (git "https://github.com/tumashu/org-webpage.git" "gh-pages")
   :site-domain "http://tumashu.github.com/org-webpage"
   :site-main-title "Org-webpage"
   :site-sub-title "(Static site senerator based on org mode)"
   :default-category "documents"
   :theme (worg killjs)
   :force-absolute-url t
   :source-browse-url ("GitHub" "https://github.com/tumashu/org-webpage")
   :personal-avatar "/media/img/horse.jpg"
   :personal-duoshuo-shortname "tumashu-website"
   :preparation-function owp/lentic-preparation-function
   :org-export-function owp/lentic-org-export-function
   :lentic-doc-sources ("org-webpage.el")
   :lentic-readme-sources ("org-webpage.el")
   :lentic-index-sources ("org-webpage.el")
   :web-server-port 6789))

(provide 'owp-devtools)

;;; owp-devtools.el ends here
