;;; org2web-template.el --- templating system based on mustache, required by org2web

;; Copyright (C)  2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'ox)
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'org2web-util)
(require 'org2web-vars)
(require 'org2web-config)
(require 'md5)


(defun org2web-get-template-file (template-file-name)
  "Get path of template file which name is `template-file-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            template-file-name)))
                          (when (file-exists-p file)
                            file)))
                    (org2web-get-theme-dirs nil nil 'templates)))))

(defun org2web-get-uploader-template (uploader-template-name)
  "Get path of uploader template which name is `uploader-template-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            uploader-template-name)))
                          (when (file-exists-p file)
                            file)))
                    (list (concat (org2web-get-repository-directory) "uploaders/")
                          (concat (file-name-as-directory org2web-load-directory)
                                  "uploaders/"
                                  (replace-regexp-in-string "/" "-" (symbol-name system-type))
                                  "/")
                          (concat (file-name-as-directory org2web-load-directory)
                                  "uploaders/common/")
                          (concat (file-name-as-directory org2web-load-directory)
                                  "uploaders/"))))))

(defun org2web-get-title (org-file)
  "Get the title of org file."
  (let ((title (org2web-read-org-option "TITLE")))
    (if (and title (> (length title) 0))
        title
      (file-name-base org-file))))

(defun org2web-get-category (org-file)
  "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. "
  (let ((func (org2web-get-config-option :retrieve-category-function)))
    (if (functionp func)
        (funcall func org-file)
      (funcall 'org2web-get-file-category org-file))))

(defun org2web-get-cache-item (key)
  "Get the item associated with KEY in `org2web-item-cache', if `org2web-item-cache' is
nil or there is no item associated with KEY in it, return nil."
  (and org2web-item-cache
       (plist-get org2web-item-cache key)))

(defun org2web-update-cache-item (key value)
  "Update the item associated with KEY in `org2web-item-cache', if `org2web-item-cache' is
nil, initialize it."
  (if org2web-item-cache
      (plist-put org2web-item-cache key value)
    (setq org2web-item-cache `(,key ,value)))
  value)

(defmacro org2web-get-cache-create (key &rest body)
  "Firstly get item from `org2web-item-cache' with KEY, if item not found, evaluate
BODY and push the result into cache and return it."
  `(or (org2web-get-cache-item ,key)
       (org2web-update-cache-item ,key (funcall (lambda () ,@body)))))

(defun org2web-render-header (&optional param-table org-file)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (org2web-get-cache-create
    :header-template
    (message "Read header.mustache from file")
    (org2web-file-to-string (org2web-get-template-file "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (funcall (org2web-get-config-option :get-title-function) org-file)
                                 " - " (org2web-get-config-option :site-main-title)))
           ("author" (or (org2web-read-org-option "AUTHOR")
                         user-full-name "Unknown Author"))
           ("description" (org2web-read-org-option "DESCRIPTION"))
           ("keywords" (org2web-read-org-option "KEYWORDS"))))))

(defun org2web-render-navigation-bar (&optional param-table org-file)
  "Render the navigation bar on each page. it will be read firstly from
`org2web-item-cache', if there is no cached content, it will be rendered
and pushed into cache from template. PARAM-TABLE is the hash table for mustache
to render the template. If it is not set or nil, this function will try to
render from a default hash table."
  (let ((site-domain (org2web-get-site-domain))
        (category-ignore-list (org2web-get-config-option :category-ignore-list)))
    (org2web-get-cache-create
     :nav-bar-html
     (message "Render navigation bar from template")
     (mustache-render
      (org2web-get-cache-create
       :nav-bar-template
       (message "Read nav.mustache from file")
       (org2web-file-to-string (org2web-get-template-file "nav.mustache")))
      (or param-table
          (ht ("site-main-title" (org2web-get-config-option :site-main-title))
              ("site-sub-title" (org2web-get-config-option :site-sub-title))
              ("nav-categories"
               (mapcar
                #'(lambda (cat)
                    (ht ("category-uri"
                         (concat "/" (org2web-encode-string-to-url cat) "/"))
                        ("category-name" (capitalize cat))))
                (sort (cl-remove-if
                       #'(lambda (cat)
                           (or (string= cat "index")
                               (string= cat "about")
                               (member cat category-ignore-list)))
                       (org2web-get-category nil))
                      'string-lessp)))
              ("nav-summary"
               (mapcar
                #'(lambda (cat)
                    (ht ("summary-item-uri"
                         (concat "/" (org2web-encode-string-to-url cat) "/"))
                        ("summary-item-name" (capitalize cat))))
                (mapcar #'car (org2web-get-config-option :summary))))
              ("nav-source-browse"
               (let ((list (org2web-get-config-option :source-browse-url)))
                 (when list
                   (ht ("source-browse-name" (car list))
                       ("source-browse-uri" (car (cdr list)))))))
              ("nav-about"
               (let ((list (org2web-get-config-option :about)))
                 (when list
                   (ht ("about-name" (car list))
                       ("about-uri" (car (cdr list)))))))
              ("nav-rss"
               (let ((list (org2web-get-config-option :rss)))
                 (when list
                   (ht ("rss-name" (car list))
                       ("rss-uri" (car (cdr list)))))))
              ("avatar" (org2web-get-config-option :personal-avatar))
              ("site-domain" (if (string-match
                                  "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                  site-domain)
                                 (match-string 1 site-domain)
                               site-domain))))))))

(defun org2web-render-content (&optional template param-table org-file)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `org2web-render-header'."
  (mustache-render
   (org2web-get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (org2web-file-to-string (org2web-get-template-file
                         (or template "post.mustache"))))
   (or param-table
       (ht ("title" (funcall (org2web-get-config-option :get-title-function) org-file))
           ("content" (cl-flet ((org-html-fontify-code
                                 (code lang)
                                 (when code (org-html-encode-plain-text code))))
                        (let ((org-export-function (org2web-get-config-option :org-export-function)))
                          (when (functionp org-export-function)
                            (funcall org-export-function)))))))))

(defun org2web-default-org-export ()
  "A function with can export org file to html."
  (org-export-as 'html nil nil t nil))

(defun org2web-render-footer (&optional param-table org-file)
  "Render the footer on each page. PARAM-TABLE is similar to
`org2web-render-header'."
  (mustache-render
   (org2web-get-cache-create
    :footer-template
    (message "Read footer.mustache from file")
    (org2web-file-to-string (org2web-get-template-file "footer.mustache")))
   (or param-table
       (let* ((site-domain (org2web-get-site-domain))
              (old-site-domain (org2web-get-site-domain t))
              (title (funcall (org2web-get-config-option :get-title-function) org-file))
              (default-category (org2web-get-config-option :default-category))
              (date (org2web-fix-timestamp-string
                     (or (org2web-read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (org2web-read-org-option "TAGS"))
              (tags (if tags  ;; Bug: when set option `:summary' to `nil', can't deal with.
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (org2web-generate-summary-uri
                                          (or (car (rassoc '(:tags) (org2web-get-config-option :summary)))
                                              "tags") tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'org2web-trim-string (split-string tags "[:,]+" t))))))
              (category (org2web-get-category org-file))
              (config (cdr (or (assoc category org2web-category-config-alist)
                               (org2web-get-category-setting default-category))))
              (uri (funcall (plist-get config :uri-generator)
                            (plist-get config :uri-template) date title)))
         (ht ("show-meta" (plist-get config :show-meta))
             ("show-comment" (and (plist-get config :show-comment)
                                  (or (org2web-get-config-option :personal-disqus-shortname)
                                      (org2web-get-config-option :personal-duoshuo-shortname))))
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
             ("author" (or (org2web-read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
             ("disqus-id" uri)
             ("disqus-url" (org2web-get-full-url uri))
             ("disqus-comment" (org2web-get-config-option :personal-disqus-shortname))
             ("disqus-shortname" (org2web-get-config-option :personal-disqus-shortname))
             ("duoshuo-thread-key" (md5 (concat
                                         (or old-site-domain site-domain)
                                         (replace-regexp-in-string " " "" title))))
             ("duoshuo-title" title)
             ("duoshuo-url" (org2web-get-full-url uri))
             ("duoshuo-comment" (org2web-get-config-option :personal-duoshuo-shortname))
             ("duoshuo-shortname" (org2web-get-config-option :personal-duoshuo-shortname))
             ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
             ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
             ("creator-info" (org2web-get-html-creator-string))
             ("email" (org2web-confound-email-address (or (org2web-read-org-option "EMAIL")
                                                          user-mail-address
                                                          "Unknown Email"))))))))


(provide 'org2web-template)

;;; org2web-template.el ends here
