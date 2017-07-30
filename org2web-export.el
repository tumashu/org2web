;;; org2web-export.el --- Publication related functions required by org2web

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

;; org source publication related functions

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'dash)
(require 'org2web-util)
(require 'org2web-vars)
(require 'org2web-config)
(require 'org2web-template)
(require 'cl-lib)


(defun org2web-publish-changes (all-files changed-files pub-root-dir)
  "This function is for:
1. publish changed org files to html
3. update index pages
4. regenerate tag pages
`all-files' contain paths of org files, `changed-files' contain org files
 to be updated.

This function don't handle deleted org-files."
  (let* ((repo-dir (org2web-get-repository-directory))
         visiting uri-alist file-attr-list)
    (when changed-files
      (dolist (org-file all-files)
        (with-temp-buffer
          (let (attr-cell)
            (insert-file-contents org-file)
            (org-mode)
            (setq attr-cell (org2web-get-org-file-options
                             org-file pub-root-dir))
            (push attr-cell file-attr-list)
            (push (cons (file-relative-name org-file repo-dir)
                        (plist-get attr-cell :relative-uri))
                  uri-alist))))
      ;; (princ file-attr-list)
      ;; (princ uri-alist)
      (dolist (org-file all-files)
        (when (member org-file changed-files)
          (with-temp-buffer
            (let (attr-cell exported-content)
              (insert-file-contents org-file)
              (org-mode)
              ;; Deal with file links, which are likes:
              ;; 1. [[file:test1.org][test1]]
              ;; 2. [[file:./test2.org][test2]]
              (mapc #'(lambda (file-link)
                        (goto-char (point-min))
                        (while (re-search-forward
                                (format "\\(file:%s\\)\\|\\(file:./%s\\)"
                                        (car file-link)
                                        (car file-link)) nil t)
                          (replace-match
                           (format "file:%s/index.html"
                                   (cdr file-link)) nil t)))
                    uri-alist)
              (setq attr-cell (org2web-get-org-file-options
                               org-file
                               pub-root-dir))
              (setq exported-content (org2web-get-org-file-export-content
                                      org-file pub-root-dir attr-cell))
              (org2web-publish-modified-file exported-content
                                         (plist-get attr-cell :pub-dir))))))
      (unless (member
               (expand-file-name "index.org" repo-dir)
               all-files)
        (org2web-generate-default-index file-attr-list pub-root-dir))
      (when (and (org2web-get-config-option :about)
                 (not (member
                       (expand-file-name "about.org" repo-dir)
                       all-files)))
        (org2web-generate-default-about pub-root-dir))
      (org2web-update-category-index file-attr-list pub-root-dir)
      (when (org2web-get-config-option :rss)
        (org2web-update-rss file-attr-list pub-root-dir))
      (mapc
       #'(lambda (name)
           (org2web-update-summary file-attr-list pub-root-dir name))
       (mapcar #'car (org2web-get-config-option :summary))))))

(defun org2web-get-org-file-options (org-file pub-root-dir)
  "Retrieve all needed options for org file opened in current buffer.
PUB-ROOT-DIR is the root directory of published files."
  (let* ((repo-dir (org2web-get-repository-directory))
         (attr-plist `(:title ,(funcall (org2web-get-config-option :get-title-function) org-file)
                              :date ,(org2web-fix-timestamp-string
                                      (or (org2web-read-org-option "DATE")
                                          (format-time-string "%Y-%m-%d")))
                              :mod-date ,(if (not org-file)
                                             (format-time-string "%Y-%m-%d")
                                           (format-time-string
                                            "%Y-%m-%d"
                                            (nth 5 (file-attributes org-file))))
                              :description ,(or (org2web-read-org-option "DESCRIPTION")
                                                "No Description")
                              :thumb ,(org2web-read-org-option "THUMBNAIL")))
         tags authors category cat-config)
    (setq tags (org2web-read-org-option "TAGS"))
    (when tags
      (plist-put
       attr-plist :tags (delete "" (mapcar 'org2web-trim-string
                                           (split-string tags "[:,]+" t)))))
    (setq authors (org2web-read-org-option "AUTHOR"))
    (when authors
      (plist-put
       attr-plist :authors (delete "" (mapcar 'org2web-trim-string
                                              (split-string authors "[:,]+" t)))))
    (setq category (org2web-get-category org-file))
    (plist-put attr-plist :category category)
    (setq cat-config (cdr (or (assoc category org2web-category-config-alist)
                              (org2web-get-category-setting
                               (org2web-get-config-option :default-category)))))
    (plist-put attr-plist :uri (funcall (plist-get cat-config :uri-generator)
                                        (plist-get cat-config :uri-template)
                                        (plist-get attr-plist :date)
                                        (plist-get attr-plist :title)))
    (plist-put attr-plist :relative-uri (replace-regexp-in-string
                                         "\\`/" ""
                                         (plist-get attr-plist :uri)))
    (plist-put attr-plist :pub-dir (file-name-as-directory
                                    (concat
                                     (file-name-as-directory pub-root-dir)
                                     (plist-get attr-plist :relative-uri))))

    attr-plist))


(defun org2web-get-org-file-export-content (org-file pub-root-dir attr-plist)
  "Export org file to html and return html content."
  (let* ((repo-dir (org2web-get-repository-directory))
         assets-dir post-content
         asset-path asset-abs-path pub-abs-path converted-path
         component-table tags authors category cat-config)
    ;; (princ attr-plist)
    (setq post-content (org2web-render-content nil nil org-file))
    (setq assets-dir (file-name-as-directory
                      (concat (file-name-as-directory pub-root-dir)
                              "assets/"
                              (plist-get attr-plist :relative-uri))))
    (with-temp-buffer
      (insert post-content)
      (goto-char (point-min))
      (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
              ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
              "<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
        (setq asset-path (match-string 2))
        (when (not (or (string-prefix-p "http://" asset-path)
                       (string-prefix-p "https://" asset-path)
                       (string-prefix-p "mailto:" asset-path)
                       (string-prefix-p "ftp://" asset-path)
                       (string-prefix-p "#" asset-path)
                       ;; TODO add more here
                       ))
          (setq asset-abs-path
                (expand-file-name asset-path (file-name-directory org-file)))
          (if (not (file-exists-p asset-abs-path))
              (message (concat "[WARN] File %s in hyper link does not exist, "
                               "org file: %s.")
                       asset-path org-file)
            (unless (file-directory-p assets-dir)
              (mkdir assets-dir t))
            (copy-file asset-abs-path assets-dir t t t t)
            (setq pub-abs-path (concat assets-dir
                                       (file-name-nondirectory asset-path)))
            (unless (string-prefix-p pub-root-dir pub-abs-path)
              (message (concat "[WARN] The publication root directory %s is not an "
                               "ancestor directory of assets directory %s.")
                       pub-root-dir assets-dir))
            (setq converted-path
                  (concat "/" (file-relative-name pub-abs-path pub-root-dir)))
            (setq post-content
                  (replace-regexp-in-string
                   (regexp-quote asset-path) converted-path post-content))))))
    (setq component-table (ht ("header" (org2web-render-header nil org-file))
                              ("nav" (org2web-render-navigation-bar nil org-file))
                              ("content" post-content)
                              ("footer" (org2web-render-footer nil org-file))))
    component-table))

(defun org2web-read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun org2web-generate-uri (default-uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%t: title of current buffer"
  (let ((uri-template (or (org2web-read-org-option "URI")
                          default-uri-template))
        (date-list (split-string (if creation-date
                                     (org2web-fix-timestamp-string creation-date)
                                   (format-time-string "%Y-%m-%d"))
                                 "-"))
        (encoded-title (org2web-encode-string-to-url title)))
    (format-spec uri-template `((?y . ,(car date-list))
                                (?m . ,(cadr date-list))
                                (?d . ,(cl-caddr date-list))
                                (?t . ,encoded-title)))))


(defun org2web-get-file-category (org-file)
  "This is the default function used to get a file's category,
see org2web config option 'retrieve-category-function. How to judge a
file's category is based on its name and its root folder name."
  (let ((repo-dir (org2web-get-repository-directory))
        (default-category (org2web-get-config-option :default-category))
        (category-ignore-list (org2web-get-config-option :category-ignore-list)))
    (cond ((not org-file)
           (let ((cat-list `("index" "about" ,(org2web-get-config-option :default-category)))) ;; 3 default categories
             (dolist (f (directory-files repo-dir))
               (when (and (not (equal f "."))
                          (not (equal f ".."))
                          (not (equal f ".git"))
                          (not (member f category-ignore-list))
                          (not (equal f default-category))
                          (file-directory-p
                           (expand-file-name f repo-dir)))
                 (setq cat-list (cons f cat-list))))
             cat-list))
          ((string= (expand-file-name "index.org" repo-dir)
                    (expand-file-name org-file)) "index")
          ((string= (expand-file-name "about.org" repo-dir)
                    (expand-file-name org-file)) "about")
          ((string= (file-name-directory (expand-file-name org-file))
                    repo-dir) default-category)
          (t (car (split-string (file-relative-name
                                 (expand-file-name org-file) repo-dir)
                                "[/\\\\]+"))))))

(defun org2web-relative-url-to-absolute (html-content)
  "Force convert relative url of `html-content' to absolute url."
  (let ((site-domain (org2web-get-site-domain))
        url)
    (if org2web-always-use-relative-url
        html-content
      (with-temp-buffer
        (insert html-content)
        (goto-char (point-min))
        (when (org2web-get-config-option :force-absolute-url)
          (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
                  ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
                  "\\(<[a-zA-Z]+[^/>]+\\)\\(src\\|href\\)\\(=\"\\)\\([^\"]+\\)\\(\"[^>]*>\\)" nil t)
            (setq url (match-string 4))
            (when (string-prefix-p "/" url)
              (setq url (concat
                         (match-string 1)
                         (match-string 2)
                         (match-string 3)
                         site-domain url
                         (match-string 5)))
              (replace-match url))))
        (buffer-string)))))

(defun org2web-publish-modified-file (component-table pub-dir)
  "Publish org file opened in current buffer. COMPONENT-TABLE is the hash table
used to render the template, PUB-DIR is the directory for published html file.
If COMPONENT-TABLE is nil, the publication will be skipped."
  (when component-table
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (org2web-string-to-file
     (org2web-relative-url-to-absolute
      (mustache-render
       (org2web-get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (org2web-file-to-string (org2web-get-template-file "container.mustache")))
       component-table))
     (concat (file-name-as-directory pub-dir) "index.html") ;; 'html-mode ;; do NOT indent the code
     )))

(defun org2web-rearrange-category-sorted (file-attr-list)
  "Rearrange and sort attribute property lists from FILE-ATTR-LIST. Rearrange
according to category, and sort according to :sort-by property defined in
`org2web-category-config-alist', if category is not in `org2web-category-config-alist',
by default, category which set by config option `:default-category' will be used.
For sorting, later lies headmost."
  (let ((default-category (org2web-get-config-option :default-category))
        cat-alist cat-list)
    (mapc
     #'(lambda (plist)
         (setq cat-list (cdr (assoc (plist-get plist :category) cat-alist)))
         (if cat-list
             (nconc cat-list (list plist))
           (setq cat-alist (cons (cons (plist-get plist :category)
                                       (list plist))
                                 cat-alist))))
     file-attr-list)
    (mapc
     #'(lambda (cell)
         (setcdr
          cell
          (sort (cdr cell)
                #'(lambda (plist1 plist2)
                    (<= (org2web-compare-standard-date
                         (org2web-fix-timestamp-string
                          (plist-get
                           plist1
                           (plist-get
                            (cdr (or (assoc (plist-get plist1 :category)
                                            org2web-category-config-alist)
                                     (org2web-get-category-setting default-category)))
                            :sort-by)))
                         (org2web-fix-timestamp-string
                          (plist-get
                           plist2
                           (plist-get
                            (cdr (or (assoc (plist-get plist2 :category)
                                            org2web-category-config-alist)
                                     (org2web-get-category-setting default-category)))
                            :sort-by))))
                        0)))))
     cat-alist)))

(defun org2web-update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (org2web-rearrange-category-sorted file-attr-list))
         (default-category (org2web-get-config-option :default-category))
         cat-dir)
    (mapc
     #'(lambda (cat-list)
         (unless (not (plist-get (cdr (or (assoc (car cat-list)
                                                 org2web-category-config-alist)
                                          (org2web-get-category-setting default-category)))
                                 :category-index))
           (setq cat-dir (file-name-as-directory
                          (concat (file-name-as-directory pub-base-dir)
                                  (org2web-encode-string-to-url (car cat-list)))))
           (unless (file-directory-p cat-dir)
             (mkdir cat-dir t))
           (org2web-string-to-file
            (org2web-relative-url-to-absolute
             (mustache-render
              (org2web-get-cache-create
               :container-template
               (message "Read container.mustache from file")
               (org2web-file-to-string (org2web-get-template-file "container.mustache")))
              (ht ("header"
                   (org2web-render-header
                    (ht ("page-title" (concat (capitalize (car cat-list))
                                              " Index - "
                                              (org2web-get-config-option :site-main-title)))
                        ("author" (or user-full-name "Unknown Author")))))
                  ("nav" (org2web-render-navigation-bar))
                  ("content"
                   (org2web-render-content
                    "category-index.mustache"
                    (ht ("cat-name" (capitalize (car cat-list)))
                        ("posts"
                         (mapcar
                          #'(lambda (attr-plist)
                              (let ((tags-multi (mapcar
                                                 #'(lambda (tag-name)
                                                     (ht ("link" (org2web-generate-summary-uri "tags" tag-name))
                                                         ("name" tag-name)))
                                                 (plist-get attr-plist :tags))))
                                (ht ("date"
                                     (plist-get
                                      attr-plist
                                      (plist-get
                                       (cdr (or (assoc
                                                 (plist-get attr-plist :category)
                                                 org2web-category-config-alist)
                                                (org2web-get-category-setting default-category)))
                                       :sort-by)))
                                    ("post-uri" (plist-get attr-plist :uri))
                                    ("post-title" (plist-get attr-plist :title))
                                    ("tag-links" (when tags-multi
                                                   (mapconcat
                                                    #'(lambda (tag)
                                                        (mustache-render
                                                         "<a href=\"{{link}}\">{{name}}</a>" tag))
                                                    tags-multi " : "))))))
                          (cdr cat-list))))))
                  ("footer"
                   (org2web-render-footer
                    (ht ("show-meta" nil)
                        ("show-comment" nil)
                        ("author" (or user-full-name "Unknown Author"))
                        ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
                        ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
                        ("creator-info" (org2web-get-html-creator-string))
                        ("email" (org2web-confound-email-address (or user-mail-address
                                                                 "Unknown Email")))))))))
            (concat cat-dir "index.html") 'html-mode)))
     sort-alist)))

(defun org2web-generate-default-index (file-attr-list pub-base-dir)
  "Generate default index page, only if index.org does not exist. FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the root
publication directory."
  (let ((sort-alist (org2web-rearrange-category-sorted file-attr-list))
        (id 0))
    (org2web-string-to-file
     (org2web-relative-url-to-absolute
      (mustache-render
       (org2web-get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (org2web-file-to-string (org2web-get-template-file "container.mustache")))
       (ht ("header"
            (org2web-render-header
             (ht ("page-title" (concat "Index - " (org2web-get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (org2web-render-navigation-bar))
           ("content"
            (org2web-render-content
             "index.mustache"
             (ht ("categories"
                  (mapcar
                   #'(lambda (cell)
                       (ht ("id" (setq id (+ id 1)))
                           ("category" (capitalize (car cell)))
                           ("posts" (mapcar
                                     #'(lambda (plist)
                                         (ht ("post-uri"
                                              (plist-get plist :uri))
                                             ("post-title"
                                              (plist-get plist :title))
                                             ("post-desc"
                                              (plist-get plist :description))
                                             ("post-date"
                                              (plist-get plist :date))
                                             ("post-thumb"
                                              (or (plist-get plist :thumb) ""))))
                                     (cdr cell)))))
                   (cl-remove-if
                    #'(lambda (cell)
                        (string= (car cell) "about"))
                    sort-alist))))))
           ("footer"
            (org2web-render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
                 ("creator-info" (org2web-get-html-creator-string))
                 ("email" (org2web-confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat (file-name-as-directory pub-base-dir) "index.html") 'html-mode)))

(defun org2web-generate-default-about (pub-base-dir)
  "Generate default about page, only if about.org does not exist. PUB-BASE-DIR
is the root publication directory."
  (let* ((about-sub-dir
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (org2web-get-config-option :about)))))
         (pub-dir (file-name-as-directory
                   (expand-file-name about-sub-dir pub-base-dir))))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (org2web-string-to-file
     (org2web-relative-url-to-absolute
      (mustache-render
       (org2web-get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (org2web-file-to-string (org2web-get-template-file "container.mustache")))
       (ht ("header"
            (org2web-render-header
             (ht ("page-title" (concat "About - " (org2web-get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (org2web-render-navigation-bar))
           ("content"
            (org2web-render-content
             "about.mustache"
             (ht ("author" (or user-full-name "Unknown Author")))))
           ("footer"
            (org2web-render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
                 ("creator-info" (org2web-get-html-creator-string))
                 ("email" (org2web-confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat pub-dir "index.html") 'html-mode)))

(defun org2web-generate-summary-uri (summary-name summary-item-name)
  "Generate summary uri based on `summary-name' and `summary-item-name'."
  (concat "/" summary-name "/" (org2web-encode-string-to-url summary-item-name) "/"))

(defun org2web-update-summary (file-attr-list pub-base-dir summary-name)
  "Update summary pages which name is `summary-name', FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the
root publication directory.

TODO: improve this function."
  (let* ((summary-base-dir (expand-file-name
                            (concat summary-name "/")
                            pub-base-dir))
         summary-alist summary-list summary-dir)
    (mapc
     #'(lambda (attr-plist)
         (mapc
          #'(lambda (name)
              (setq summary-list (assoc name summary-alist))
              (unless summary-list
                (add-to-list 'summary-alist (setq summary-list `(,name))))
              (nconc summary-list (list attr-plist)))
          (let* ((summary-attr (car (cdr (assoc summary-name (org2web-get-config-option :summary)))))
                 (elem (plist-get attr-plist summary-attr)))
            (if (listp elem) elem (list elem)))))
     file-attr-list)
    (unless (file-directory-p summary-base-dir)
      (mkdir summary-base-dir t))
    (org2web-string-to-file
     (org2web-relative-url-to-absolute
      (mustache-render
       (org2web-get-cache-create
        :container-template
        (message "Read container.mustache from file")
        (org2web-file-to-string (org2web-get-template-file "container.mustache")))
       (ht ("header"
            (org2web-render-header
             (ht ("page-title" (concat (capitalize summary-name)
                                       " Index - "
                                       (org2web-get-config-option :site-main-title)))
                 ("author" (or user-full-name "Unknown Author")))))
           ("nav" (org2web-render-navigation-bar))
           ("content"
            (org2web-render-content
             "summary-index.mustache"
             (ht ("summary-name" (capitalize summary-name))
                 ("summary"
                  (mapcar
                   #'(lambda (summary-list)
                       (ht ("summary-item-name" (car summary-list))
                           ("summary-item-uri" (org2web-generate-summary-uri summary-name (car summary-list)))
                           ("count" (number-to-string (length (cdr summary-list))))))
                   summary-alist)))))
           ("footer"
            (org2web-render-footer
             (ht ("show-meta" nil)
                 ("show-comment" nil)
                 ("author" (or user-full-name "Unknown Author"))
                 ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
                 ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
                 ("creator-info" (org2web-get-html-creator-string))
                 ("email" (org2web-confound-email-address (or user-mail-address
                                                          "Unknown Email")))))))))
     (concat summary-base-dir "index.html") 'html-mode)
    (mapc
     #'(lambda (summary-list)
         (setq summary-dir (file-name-as-directory
                            (concat summary-base-dir
                                    (org2web-encode-string-to-url (car summary-list)))))
         (unless (file-directory-p summary-dir)
           (mkdir summary-dir t))
         (org2web-string-to-file
          (org2web-relative-url-to-absolute
           (mustache-render
            (org2web-get-cache-create
             :container-template
             (message "Read container.mustache from file")
             (org2web-file-to-string (org2web-get-template-file "container.mustache")))
            (ht ("header"
                 (org2web-render-header
                  (ht ("page-title" (concat (capitalize summary-name) ": " (car summary-list)
                                            " - " (org2web-get-config-option :site-main-title)))
                      ("author" (or user-full-name "Unknown Author")))))
                ("nav" (org2web-render-navigation-bar))
                ("content"
                 (org2web-render-content
                  "summary.mustache"
                  (ht ("summary-name" (capitalize summary-name))
                      ("summary-item-name" (car summary-list))
                      ("posts"
                       (mapcar
                        #'(lambda (attr-plist)
                            (ht ("post-uri" (plist-get attr-plist :uri))
                                ("post-title" (plist-get attr-plist :title))
                                ("post-date" (plist-get attr-plist :date))))
                        (cdr summary-list))))))
                ("footer"
                 (org2web-render-footer
                  (ht ("show-meta" nil)
                      ("show-comment" nil)
                      ("author" (or user-full-name "Unknown Author"))
                      ("google-analytics" (org2web-get-config-option :personal-google-analytics-id))
                      ("google-analytics-id" (org2web-get-config-option :personal-google-analytics-id))
                      ("creator-info" (org2web-get-html-creator-string))
                      ("email" (org2web-confound-email-address (or user-mail-address
                                                               "Unknown Email")))))))))
          (concat summary-dir "index.html") 'html-mode))
     summary-alist)))

(defun org2web-update-rss (file-attr-list pub-base-dir)
  "Update RSS. FILE-ATTR-LIST is the list of all file attribute property lists.
PUB-BASE-DIR is the root publication directory."
  (let* ((rss-file-name
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (org2web-get-config-option :rss)))))
         (rss-file
          (concat (file-name-as-directory pub-base-dir) rss-file-name))
         (rss-base-dir
          (file-name-directory rss-file))
         (last-10-posts
          (-take 10 (--sort (>= 0 (org2web-compare-standard-date
                                   (org2web-fix-timestamp-string
                                    (plist-get it :mod-date))
                                   (org2web-fix-timestamp-string
                                    (plist-get other :mod-date))))
                            (--filter (not (or
                                            (string= (plist-get it :category)
                                                     "index")
                                            (string= (plist-get it :category)
                                                     "about")))
                                      file-attr-list)))))
    (unless (file-directory-p rss-base-dir)
      (mkdir rss-base-dir t))
    (org2web-string-to-file
     (org2web-relative-url-to-absolute
      (mustache-render
       org2web-rss-template
       (ht ("title" (org2web-get-config-option :site-main-title))
           ("link" (org2web-get-site-domain))
           ("description" (org2web-get-config-option :site-sub-title))
           ("date" (format-time-string "%a, %d %b %Y %T %Z"))
           ("items" (--map (ht ("item-title" (plist-get it :title))
                               ("item-link" (org2web-get-full-url (plist-get it :uri)))
                               ("item-description" (plist-get it :description))
                               ("item-update-date" (plist-get it :mod-date)))
                           last-10-posts)))))
     rss-file)))


(provide 'org2web-export)

;;; org2web-export.el ends here
