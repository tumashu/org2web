;;; org2web-el2org.el --- el2org support for org2web

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu  <tumashu AT 163.com>
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

;; * 说明文档                                                           :README:

;; #+BEGIN_EXAMPLE
;; (require 'org2web-el2org)
;; #+END_EXAMPLE

;;; Code:

;; * 代码                                                                 :code:

;; ** Require
(require 'el2org)

(defun org2web-el2org-generate-readme (&optional project-name)
  (interactive)
  (org2web-select-project
   "Which project do you want to generate README.md? " project-name)
  (let* ((repo-dir (org2web-get-repository-repo-directory))
         (el-file (concat
                   (file-name-as-repo-directory repo-dir)
                   (car (org2web-get-config-option :el2org-readme-sources))))
         (readme-file (concat (file-name-as-repo-directory repo-dir) "README.md"))
         (tags (org2web-get-config-option :el2org-readme-tags)))
    (if (featurep 'ox-gfm)
        (el2org-generate-file el-file tags 'gfm readme-file)
      (el2org-generate-file el-file tags 'md readme-file))))

(defun org2web-el2org-generate-index (&optional project-name)
  (interactive)
  (org2web-select-project
   "Which project do you want to generate index.org? " project-name)
  (let* ((repo-dir (org2web-get-repository-repo-directory))
         (el-file (concat
                   (file-name-as-repo-directory repo-dir)
                   (car (org2web-get-config-option :el2org-readme-sources))))
         (index-file (concat (file-name-as-repo-directory repo-dir) "index.org"))
         (tags (org2web-get-config-option :el2org-readme-tags)))
    (el2org-generate-file el-file tags 'org index-file)))

;; ** org2web 导出函数（支持 el2org）

(defun org2web-el2org-org-export-function ()
  "A function with can export org file to html."
  (let ((org-export-headline-levels 7)
        (indent-tabs-mode nil)
        (tab-width 4))
    (org-export-as 'html nil nil t nil)))

(defun org2web-el2org-preparation-function ()
  "Generate org files by el2org."
  ;; Orgify elisp files
  (let* ((repo-dir (org2web-get-repository-directory))
         (tags (org2web-get-config-option :el2org-doc-tags))
         (regexp-list (org2web-get-config-option :el2org-doc-sources))
         (files (when regexp-list
                  (org2web-select-matched-items
                   (org2web-directory-files-recursively repo-dir "\\.el$")
                   regexp-list))))
    (when files
      (mapc
       #'(lambda (file)
           (when (file-exists-p file)
             (let ((org-file (concat (file-name-sans-extension file) ".org")))
               (el2org-generate-file file tags 'org org-file))))
       files)))

  ;; Generate README.md if necessary
  (let* ((repo-dir (org2web-get-repository-directory))
         (filename (car (org2web-get-config-option :el2org-readme-sources)))
         (file (when filename
                 (concat (file-name-as-directory repo-dir) filename)))
         (readme-file (concat (file-name-as-directory repo-dir) "README.md"))
         (tags (org2web-get-config-option :el2org-readme-tags)))
    (if (featurep 'ox-gfm)
        (el2org-generate-file file tags 'gfm readme-file)
      (el2org-generate-file file tags 'md readme-file)))

  ;; Generate index.org if necessary
  (let* ((repo-dir (org2web-get-repository-directory))
         (filename (car (org2web-get-config-option :el2org-index-sources)))
         (file (when filename
                 (concat (file-name-as-directory repo-dir) filename)))
         (index-file (concat (file-name-as-directory repo-dir) "index.org"))
         (tags (org2web-get-config-option :el2org-index-tags)))
    (el2org-generate-file file tags 'org index-file)))

(define-obsolete-function-alias 'owp/el2org-preparation-function 'org2web-el2org-preparation-function "0.1")
(define-obsolete-function-alias 'owp/el2org-org-export-function 'org2web-el2org-org-export-function "0.1")

;; * Footer

(provide 'org2web-el2org)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; org2web-el2org.el ends here
