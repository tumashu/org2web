;;; owp-config.el --- Functions dealing with org-webpage configure

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

;; owp-config.el contains functions used to deal with org-webpage configure.

;;; Code:

(require 'owp-vars)

(defun owp/get-config-option (option)
  "The function used to read org-webpage config"
  (when (functionp owp/get-config-option-function)
    (let ((output (funcall owp/get-config-option-function option)))
      ;; if "output" is a form which like (:eval myform),
      ;; eval myform and return the result, otherwise
      ;; return "output".
      (if (and (listp output)
               (eq (car output) :eval))
          (eval `(progn ,@(cdr output)))
        output))))

(defun owp/get-config-option-from-alist (option)
  "The default org-webpage config read function,
which can read `option' from `owp/project-config-alist'
if `option' is not found, get fallback value from
`owp/config-fallback'."
  (let ((project-plist (cdr (assoc owp/current-project-name
                                   owp/project-config-alist))))
    (if (plist-member project-plist option)
        (plist-get project-plist option)
      (plist-get owp/config-fallback option))))

(defun owp/get-repository-directory ()
  "The function, which can return repository directory string."
  (let ((dir (owp/get-config-option :repository-directory)))
    (when dir
      (file-name-as-directory
       (expand-file-name dir)))))

(defun owp/get-publishing-directory ()
  "The function, which can return publishing directory string."
  (let ((dir (owp/get-config-option :publishing-directory)))
    (when dir
      (file-name-as-directory
       (expand-file-name dir)))))

(defun owp/get-site-domain ()
  "The function, which can return site-domain string."
  (let ((site-domain (owp/get-config-option :site-domain)))
    (when site-domain
      (if (or (string-prefix-p "http://"  site-domain)
              (string-prefix-p "https://" site-domain))
          (directory-file-name
           (file-name-as-directory site-domain))
        (directory-file-name
         (file-name-as-directory
          (concat "http://" site-domain)))))))

(defun owp/get-theme-dirs (&optional root-dir theme type)
  "The function ,return org-webpage theme type paths list.

org-webpage organizes its themes by directory:

| Directory           |  Argument   |  Value                 |
+---------------------+-------------+------------------------+
| /path/to/directory  |  <root-dir> | \"/path/to/directory\" |
|  \--mdo             |  <theme>    | 'mdo                   |
|      |-- templates  |  <type>     | 'templates             |
|       \- resources  |  <type>     | 'resources             |

`root-dir' and `theme' can be lists, for example:

  `(\"path/to/dir1\" \"path/to/dir2\" \"path/to/dir3\")'
  `(theme1 theme2 theme3)'

At this time, `owp/get-theme-dirs' will find *all possible*
<type> directorys by permutation way and return a list with
multi path."
  (let* ((themes (delete-dups
                  (if theme
                      (list theme)
                    `(,@(owp/get-config-option :theme) default))))
         (theme-root-dirs (delete-dups
                           (if root-dir
                               (list root-dir)
                             `(,@(owp/get-config-option :theme-root-directory)
                               ,(concat (owp/get-repository-directory) "themes/")
                               ,(concat owp/load-directory "themes/")))))
         theme-dir theme-dirs)
    (dolist (theme themes)
      (dolist (root-dir theme-root-dirs)
        (setq theme-dir
              (file-name-as-directory
               (expand-file-name
                (format "%s/%s" (symbol-name theme)
                        (if type (symbol-name type) ""))
                root-dir)))
        (when (file-directory-p theme-dir)
          (push theme-dir theme-dirs))))
    (reverse theme-dirs)))

(defun owp/get-html-creator-string ()
  "The function, which can return creator string."
  (or (owp/get-config-option :html-creator-string) ""))

(defun owp/get-category-setting (category)
  "The function , which can return category config of `category'"
  (or (assoc category owp/category-config-alist)
      `(,category
        :show-meta t
        :show-comment t
        :uri-generator owp/generate-uri
        :uri-template ,(format "/%s/%%t/" category)
        :sort-by :date
        :category-index t)))

(provide 'owp-config)

;;; owp-config.el ends here
