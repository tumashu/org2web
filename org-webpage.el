;;; org-webpage.el --- Please install "org2web" instead.

;; * Header
;; Copyright (C)  2016 Feng Shu, Jorge Javier Araya Navarro
;;                2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Jorge Javier Araya Navarro <elcorreo AT deshackra.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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
;; Don't use this package, please install and use "org2web" package instead.
;;; Code:

;; * 代码说明                                                             :code:
(require 'org2web)

(define-obsolete-variable-alias 'owp/default-project-name 'org2web-default-project)
(define-obsolete-variable-alias 'owp/temporary-directory 'org2web-temporary-directory)
(define-obsolete-variable-alias 'owp/terminal-emulater 'org2web-terminal-emulater)
(define-obsolete-variable-alias 'owp/uploader-config-alist 'org2web-uploaders)
(define-obsolete-variable-alias 'owp/project-config-alist 'org2web-projects)
(define-obsolete-variable-alias 'owp/get-config-option-function 'org2web-get-config-option-function)

(define-obsolete-function-alias 'owp/add-project-config 'org2web-add-project)
(define-obsolete-function-alias 'owp/select-project-name 'org2web-select-project)
(define-obsolete-function-alias 'owp/do-publication 'org2web-publish)
(define-obsolete-function-alias 'owp/new-post 'org2web-new-post)

(define-obsolete-function-alias 'owp/el2org-preparation-function 'org2web-el2org-preparation-function)
(define-obsolete-function-alias 'owp/el2org-org-export-function 'org2web-el2org-org-export-function)

;; * Footer
(provide 'org-webpage)

;;; org-webpage.el ends here
