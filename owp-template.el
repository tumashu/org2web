;;; owp-template.el --- templating system based on mustache, required by org-webpage

;; Copyright (C)  2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'ox)
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'owp-util)
(require 'owp-vars)
(require 'owp-config)


(defun owp/get-template-file (template-file-name)
  "Get path of template file which name is `template-file-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            template-file-name)))
                          (when (file-exists-p file)
                            file)))
                    (owp/get-theme-dirs nil nil 'templates)))))

(defun owp/get-upload-script-file (upload-script-file-name)
  "Get path of upload script file which name is `upload-script-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            upload-script-file-name)))
                          (when (file-exists-p file)
                            file)))
                    (list (concat (file-name-as-directory owp/load-directory)
                                  "upload-scripts/"
                                  (replace-regexp-in-string "/" "-" (symbol-name system-type))
                                  "/")
                          (concat (file-name-as-directory owp/load-directory)
                                  "upload-scripts/common/"))))))

(defun owp/get-title (org-file)
  "Get the title of org file."
  (let ((title (owp/read-org-option "TITLE")))
    (if (and title (> (length title) 0))
        title
      (file-name-base org-file))))

(defun owp/get-category (org-file)
  "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. "
  (let ((func (owp/get-config-option :retrieve-category-function)))
    (if (functionp func)
        (funcall func org-file)
      (funcall 'owp/get-file-category org-file))))

(defun owp/get-cache-item (key)
  "Get the item associated with KEY in `owp/item-cache', if `owp/item-cache' is
nil or there is no item associated with KEY in it, return nil."
  (and owp/item-cache
       (plist-get owp/item-cache key)))

(defun owp/update-cache-item (key value)
  "Update the item associated with KEY in `owp/item-cache', if `owp/item-cache' is
nil, initialize it."
  (if owp/item-cache
      (plist-put owp/item-cache key value)
    (setq owp/item-cache `(,key ,value)))
  value)

(defmacro owp/get-cache-create (key &rest body)
  "Firstly get item from `owp/item-cache' with KEY, if item not found, evaluate
BODY and push the result into cache and return it."
  `(or (owp/get-cache-item ,key)
       (owp/update-cache-item ,key (funcall (lambda () ,@body)))))

(defun owp/render-header (&optional param-table org-file)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (owp/get-cache-create
    :header-template
    (message "Read header.mustache from file")
    (owp/file-to-string (owp/get-template-file "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (funcall (owp/get-config-option :get-title-function) org-file)
                                 " - " (owp/get-config-option :site-main-title)))
           ("author" (or (owp/read-org-option "AUTHOR")
                         user-full-name "Unknown Author"))
           ("description" (owp/read-org-option "DESCRIPTION"))
           ("keywords" (owp/read-org-option "KEYWORDS"))))))

(defun owp/render-navigation-bar (&optional param-table org-file)
  "Render the navigation bar on each page. it will be read firstly from
`owp/item-cache', if there is no cached content, it will be rendered
and pushed into cache from template. PARAM-TABLE is the hash table for mustache
to render the template. If it is not set or nil, this function will try to
render from a default hash table."
  (let ((site-domain (owp/get-site-domain)))
    (owp/get-cache-create
     :nav-bar-html
     (message "Render navigation bar from template")
     (mustache-render
      (owp/get-cache-create
       :nav-bar-template
       (message "Read nav.mustache from file")
       (owp/file-to-string (owp/get-template-file "nav.mustache")))
      (or param-table
          (ht ("site-main-title" (owp/get-config-option :site-main-title))
              ("site-sub-title" (owp/get-config-option :site-sub-title))
              ("nav-categories"
               (mapcar
                #'(lambda (cat)
                    (ht ("category-uri"
                         (concat "/" (owp/encode-string-to-url cat) "/"))
                        ("category-name" (capitalize cat))))
                (sort (cl-remove-if
                       #'(lambda (cat)
                           (or (string= cat "index")
                               (string= cat "about")))
                       (owp/get-category nil))
                      'string-lessp)))
              ("nav-summary"
               (mapcar
                #'(lambda (cat)
                    (ht ("summary-item-uri"
                         (concat "/" (owp/encode-string-to-url cat) "/"))
                        ("summary-item-name" (capitalize cat))))
                (mapcar #'car (owp/get-config-option :summary))))
              ("nav-source-browse"
               (let ((list (owp/get-config-option :source-browse-url)))
                 (when list
                   (ht ("source-browse-name" (car list))
                       ("source-browse-uri" (car (cdr list)))))))
              ("nav-about"
               (let ((list (owp/get-config-option :about)))
                 (when list
                   (ht ("about-name" (car list))
                       ("about-uri" (car (cdr list)))))))
              ("nav-rss"
               (let ((list (owp/get-config-option :rss)))
                 (when list
                   (ht ("rss-name" (car list))
                       ("rss-uri" (car (cdr list)))))))
              ("avatar" (owp/get-config-option :personal-avatar))
              ("site-domain" (if (string-match
                                  "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                  site-domain)
                                 (match-string 1 site-domain)
                               site-domain))))))))

(defun owp/render-content (&optional template param-table org-file)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `owp/render-header'."
  (mustache-render
   (owp/get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (owp/file-to-string (owp/get-template-file
                         (or template "post.mustache"))))
   (or param-table
       (ht ("title" (funcall (owp/get-config-option :get-title-function) org-file))
           ("content" (cl-flet ((org-html-fontify-code
                                 (code lang)
                                 (when code (org-html-encode-plain-text code))))
                        (let ((org-export-function (owp/get-config-option :org-export-function)))
                          (when (functionp org-export-function)
                            (funcall org-export-function)))))))))

(defun owp/default-org-export ()
  "A function with can export org file to html."
  (org-export-as 'html nil nil t nil))

(defun owp/render-footer (&optional param-table org-file)
  "Render the footer on each page. PARAM-TABLE is similar to
`owp/render-header'."
  (mustache-render
   (owp/get-cache-create
    :footer-template
    (message "Read footer.mustache from file")
    (owp/file-to-string (owp/get-template-file "footer.mustache")))
   (or param-table
       (let* ((title (funcall (owp/get-config-option :get-title-function) org-file))
              (default-category (owp/get-config-option :default-category))
              (date (owp/fix-timestamp-string
                     (or (owp/read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (owp/read-org-option "TAGS"))
              (tags (if tags  ;; Bug: when set option `:summary' to `nil', can't deal with.
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (owp/generate-summary-uri
                                          (or (car (rassoc '(:tags) (owp/get-config-option :summary)))
                                              "tags") tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'owp/trim-string (split-string tags "[:,]+" t))))))
              (category (owp/get-category org-file))
              (config (cdr (or (assoc category owp/category-config-alist)
                               (owp/get-category-setting default-category))))
              (uri (funcall (plist-get config :uri-generator)
                            (plist-get config :uri-template) date title)))
         (ht ("show-meta" (plist-get config :show-meta))
             ("show-comment" (plist-get config :show-comment))
             ("date" date)
             ("mod-date" (if (not org-file)
                             (format-time-string "%Y-%m-%d")
                           (format-time-string
                            "%Y-%m-%d"
                            (nth 5 (file-attributes org-file)))))
             ("tags" tags)
             ("tag-links" (if (not tags) "N/A"
                            (mapconcat
                             #'(lambda (tag)
                                 (mustache-render
                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                             tags " ")))
             ("author" (or (owp/read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
             ("disqus-id" uri)
             ("disqus-url" (owp/get-full-url uri))
             ("disqus-comment" (owp/get-config-option :personal-disqus-shortname))
             ("disqus-shortname" (owp/get-config-option :personal-disqus-shortname))
             ("duoshuo-comment" (owp/get-config-option :personal-duoshuo-shortname))
             ("duoshuo-shortname" (owp/get-config-option :personal-duoshuo-shortname))
             ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
             ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
             ("creator-info" (owp/get-html-creator-string))
             ("email" (owp/confound-email-address (or (owp/read-org-option "EMAIL")
                                                      user-mail-address
                                                      "Unknown Email"))))))))


(provide 'owp-template)

;;; owp-template.el ends here
