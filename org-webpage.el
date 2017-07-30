;;; org-webpage.el --- Please install "owp" instead.

;; * Header
;; Copyright (C)  2016 Feng Shu, Jorge Javier Araya Navarro
;;                2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Jorge Javier Araya Navarro <elcorreo AT deshackra.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
;; Package-requires: (("owp" 0.1))
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
;; Don't use this package, please use "owp" package instead.
;;; Code:

;; * 代码说明                                                             :code:
(require 'owp)

(defun owp/add-project-config (project-config))
(defun owp/select-project-name (prompt &optional project-name))
(defun owp/do-publication (&optional project-name publishing-directory job-number update-top-n))
(defun owp/new-post (&optional project-name category filename insert-fallback-template))

;; * Footer
(provide 'org-webpage)

;;; org-webpage.el ends here
