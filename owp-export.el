;;; owp-export.el --- Publication related functions required by org-webpage

;; Copyright (C)  2005 Feng Shu
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

;; org source publication related functions

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'dash)
(require 'owp-util)
(require 'owp-vars)
(require 'owp-config)
(require 'owp-git)
(require 'owp-template)


(defun owp/publish-changes (all-list change-plist pub-root-dir)
  "This function is for:
1. publish changed org files to html
2. delete html files which are relevant to deleted org files (NOT implemented)
3. update index pages
4. regenerate tag pages
ALL-LIST contains paths of all org files, CHANGE-PLIST contains two properties,
one is :update for files to be updated, another is :delete for files to be
deleted. PUB-ROOT-DIR is the root publication directory."
  (let* ((repo-dir (owp/get-repository-directory))
         (upd-list (plist-get change-plist :update))
         (del-list (plist-get change-plist :delete))
         visiting file-buffer attr-cell file-attr-list)
    (when (or upd-list del-list)
      (mapc
       #'(lambda (org-file)
           (setq visiting (find-buffer-visiting org-file))
           (with-current-buffer (setq file-buffer
                                      (or visiting (find-file org-file)))
             (setq attr-cell (owp/get-org-file-options
                              pub-root-dir
                              (member org-file upd-list)))
             (setq file-attr-list (cons (car attr-cell) file-attr-list))
             (when (member org-file upd-list)
               (owp/publish-modified-file (cdr attr-cell)
                                         (plist-get (car attr-cell) :pub-dir)))
             (when (member org-file del-list)
               (owp/handle-deleted-file org-file)))
           (or visiting (kill-buffer file-buffer)))
       all-list)
      (unless (member
               (expand-file-name "index.org" repo-dir)
               all-list)
        (owp/generate-default-index file-attr-list pub-root-dir))
      (unless (member
               (expand-file-name "about.org" repo-dir)
               all-list)
        (owp/generate-default-about pub-root-dir))
      (owp/update-category-index file-attr-list pub-root-dir)
      (owp/update-rss file-attr-list pub-root-dir)
      (owp/update-tags file-attr-list pub-root-dir))))

(defun owp/get-org-file-options (pub-root-dir do-pub)
  "Retrieve all needed options for org file opened in current buffer.
PUB-ROOT-DIR is the root directory of published files, if DO-PUB is t, the
content of the buffer will be converted into html."
  (let* ((repo-dir (owp/get-repository-directory))
         (filename (buffer-file-name))
         (attr-plist `(:title ,(funcall (owp/get-config-option :get-title-function))
                              :date ,(owp/fix-timestamp-string
                                      (or (owp/read-org-option "DATE")
                                          (format-time-string "%Y-%m-%d")))
                              :mod-date ,(if (not filename)
                                             (format-time-string "%Y-%m-%d")
                                           (or (owp/git-last-change-date
                                                repo-dir
                                                filename)
                                               (format-time-string
                                                "%Y-%m-%d"
                                                (nth 5 (file-attributes filename)))))
                              :description ,(or (owp/read-org-option "DESCRIPTION")
                                                "No Description")
                              :thumb ,(owp/read-org-option "THUMBNAIL")))
         assets-dir post-content
         asset-path asset-abs-path pub-abs-path converted-path
         component-table tags category cat-config)
    (setq tags (owp/read-org-option "TAGS"))
    (when tags
      (plist-put
       attr-plist :tags (delete "" (mapcar 'owp/trim-string
                                           (split-string tags "[:,]+" t)))))
    (setq category (funcall (or (owp/get-config-option :retrieve-category-function)
                                owp/get-file-category)
                            filename))
    (plist-put attr-plist :category category)
    (setq cat-config (cdr (or (assoc category owp/category-config-alist)
                              (owp/get-category-setting
                               (owp/get-config-option :default-category)))))
    (plist-put attr-plist :uri (funcall (plist-get cat-config :uri-generator)
                                        (plist-get cat-config :uri-template)
                                        (plist-get attr-plist :date)
                                        (plist-get attr-plist :title)))
    (plist-put attr-plist :pub-dir (file-name-as-directory
                                    (concat
                                     (file-name-as-directory pub-root-dir)
                                     (replace-regexp-in-string
                                      "\\`/" ""
                                      (plist-get attr-plist :uri)))))
    (when do-pub
      (princ attr-plist)
      (setq post-content (owp/render-content))
      (setq assets-dir (file-name-as-directory
                        (concat (file-name-as-directory pub-root-dir)
                                "assets/"
                                (replace-regexp-in-string
                                 "\\`" "" (plist-get attr-plist :uri)))))
      (with-temp-buffer
        (insert post-content)
        (beginning-of-buffer)
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
                  (expand-file-name asset-path (file-name-directory filename)))
            (if (not (file-exists-p asset-abs-path))
                (message "[WARN] File %s in hyper link does not exist, org \
file: %s." asset-path filename)
              (unless (file-directory-p assets-dir)
                (mkdir assets-dir t))
              (copy-file asset-abs-path assets-dir t t t t)
              (setq pub-abs-path (concat assets-dir
                                         (file-name-nondirectory asset-path)))
              (unless (string-prefix-p pub-root-dir pub-abs-path)
                (message "[WARN] The publication root directory %s is not an \
ancestor directory of assets directory %s." pub-root-dir assets-dir))
              (setq converted-path
                    (concat "/" (file-relative-name pub-abs-path pub-root-dir)))
              (setq post-content
                    (replace-regexp-in-string
                     (regexp-quote asset-path) converted-path post-content))))))
      (setq component-table (ht ("header" (owp/render-header))
                                ("nav" (owp/render-navigation-bar))
                                ("content" post-content)
                                ("footer" (owp/render-footer)))))
    (cons attr-plist component-table)))

(defun owp/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun owp/generate-uri (default-uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%t: title of current buffer"
  (let ((uri-template (or (owp/read-org-option "URI")
                          default-uri-template))
        (date-list (split-string (if creation-date
                                     (owp/fix-timestamp-string creation-date)
                                   (format-time-string "%Y-%m-%d"))
                                 "-"))
        (encoded-title (owp/encode-string-to-url title)))
    (format-spec uri-template `((?y . ,(car date-list))
                                (?m . ,(cadr date-list))
                                (?d . ,(caddr date-list))
                                (?t . ,encoded-title)))))

(defun owp/get-file-category (org-file)
  "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. This is the default function used to get a file's category,
see org-webpage config option 'retrieve-category-function. How to judge a
file's category is based on its name and its root folder name."
  (let ((repo-dir (owp/get-repository-directory))
        (default-category (owp/get-config-option :default-category))
        (category-ignore-list (owp/get-config-option :category-ignore-list)))
    (cond ((not org-file)
           (let ((cat-list `("index" "about" ,(owp/get-config-option :default-category)))) ;; 3 default categories
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

(defun owp/publish-modified-file (component-table pub-dir)
  "Publish org file opened in current buffer. COMPONENT-TABLE is the hash table
used to render the template, PUB-DIR is the directory for published html file.
If COMPONENT-TABLE is nil, the publication will be skipped."
  (when component-table
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (owp/string-to-file (mustache-render
                     (owp/get-cache-create
                      :container-template
                      (message "Read container.mustache from file")
                      (owp/file-to-string (owp/get-template-file "container.mustache")))
                     component-table)
                    (concat pub-dir "index.html") ;; 'html-mode ;; do NOT indent the code
                    )))

(defun owp/handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )

(defun owp/rearrange-category-sorted (file-attr-list)
  "Rearrange and sort attribute property lists from FILE-ATTR-LIST. Rearrange
according to category, and sort according to :sort-by property defined in
`owp/category-config-alist', if category is not in `owp/category-config-alist',
by default, category which set by config option `:default-category' will be used.
For sorting, later lies headmost."
  (let ((default-category (owp/get-config-option :default-category))
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
                    (<= (owp/compare-standard-date
                         (owp/fix-timestamp-string
                          (plist-get
                           plist1
                           (plist-get
                            (cdr (or (assoc (plist-get plist1 :category)
                                            owp/category-config-alist)
                                     (owp/get-category-setting default-category)))
                            :sort-by)))
                         (owp/fix-timestamp-string
                          (plist-get
                           plist2
                           (plist-get
                            (cdr (or (assoc (plist-get plist2 :category)
                                            owp/category-config-alist)
                                     (owp/get-category-setting default-category)))
                            :sort-by))))
                        0)))))
     cat-alist)))

(defun owp/update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (owp/rearrange-category-sorted file-attr-list))
         (default-category (owp/get-config-option :default-category))
         cat-dir)
    (mapc
     #'(lambda (cat-list)
         (unless (not (plist-get (cdr (or (assoc (car cat-list)
                                                 owp/category-config-alist)
                                          (owp/get-category-setting default-category)))
                                 :category-index))
           (setq cat-dir (file-name-as-directory
                          (concat (file-name-as-directory pub-base-dir)
                                  (owp/encode-string-to-url (car cat-list)))))
           (unless (file-directory-p cat-dir)
             (mkdir cat-dir t))
           (owp/string-to-file
            (mustache-render
             (owp/get-cache-create
              :container-template
              (message "Read container.mustache from file")
              (owp/file-to-string (owp/get-template-file "container.mustache")))
             (ht ("header"
                  (owp/render-header
                   (ht ("page-title" (concat (capitalize (car cat-list))
                                             " Index - "
                                             (owp/get-config-option :site-main-title)))
                       ("author" (or user-full-name "Unknown Author")))))
                 ("nav" (owp/render-navigation-bar))
                 ("content"
                  (owp/render-content
                   "category-index.mustache"
                   (ht ("cat-name" (capitalize (car cat-list)))
                       ("posts"
                        (mapcar
                         #'(lambda (attr-plist)
                             (ht ("date"
                                  (plist-get
                                   attr-plist
                                   (plist-get
                                    (cdr (or (assoc
                                              (plist-get attr-plist :category)
                                              owp/category-config-alist)
                                             (owp/get-category-setting default-category)))
                                    :sort-by)))
                                 ("post-uri" (plist-get attr-plist :uri))
                                 ("post-title" (plist-get attr-plist :title))))
                         (cdr cat-list))))))
                 ("footer"
                  (owp/render-footer
                   (ht ("show-meta" nil)
                       ("show-comment" nil)
                       ("author" (or user-full-name "Unknown Author"))
                       ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
                       ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
                       ("creator-info" (owp/get-html-creator-string))
                       ("email" (owp/confound-email-address (or user-mail-address
                                                               "Unknown Email"))))))))
            (concat cat-dir "index.html") 'html-mode)))
     sort-alist)))

(defun owp/generate-default-index (file-attr-list pub-base-dir)
  "Generate default index page, only if index.org does not exist. FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the root
publication directory."
  (let ((sort-alist (owp/rearrange-category-sorted file-attr-list))
        (id 0))
    (owp/string-to-file
     (mustache-render
      (owp/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (owp/file-to-string (owp/get-template-file "container.mustache")))
      (ht ("header"
           (owp/render-header
            (ht ("page-title" (concat "Index - " (owp/get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (owp/render-navigation-bar))
          ("content"
           (owp/render-content
            "index.mustache"
            (ht ("categories"
                 (mapcar
                  #'(lambda (cell)
                      (ht ("id" (setq id (+ id 1)))
                          ("category" (car cell))
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
                  sort-alist)))))
          ("footer"
           (owp/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
                ("creator-info" (owp/get-html-creator-string))
                ("email" (owp/confound-email-address (or user-mail-address
                                                        "Unknown Email"))))))))
     (concat (file-name-as-directory pub-base-dir) "index.html") 'html-mode)))

(defun owp/generate-default-about (pub-base-dir)
  "Generate default about page, only if about.org does not exist. PUB-BASE-DIR
is the root publication directory."
  (let ((pub-dir (file-name-as-directory
                  (expand-file-name "about/" pub-base-dir))))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (owp/string-to-file
     (mustache-render
      (owp/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (owp/file-to-string (owp/get-template-file "container.mustache")))
      (ht ("header"
           (owp/render-header
            (ht ("page-title" (concat "About - " (owp/get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (owp/render-navigation-bar))
          ("content"
           (owp/render-content
            "about.mustache"
            (ht ("author" (or user-full-name "Unknown Author")))))
          ("footer"
           (owp/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
                ("creator-info" (owp/get-html-creator-string))
                ("email" (owp/confound-email-address (or user-mail-address
                                                        "Unknown Email"))))))))
     (concat pub-dir "index.html") 'html-mode)))

(defun owp/generate-tag-uri (tag-name)
  "Generate tag uri based on TAG-NAME."
  (concat "/tags/" (owp/encode-string-to-url tag-name) "/"))

(defun owp/update-tags (file-attr-list pub-base-dir)
  "Update tag pages. FILE-ATTR-LIST is the list of all file attribute property
lists. PUB-BASE-DIR is the root publication directory.
TODO: improve this function."
  (let ((tag-base-dir (expand-file-name "tags/" pub-base-dir))
        tag-alist tag-list tag-dir)
    (mapc
     #'(lambda (attr-plist)
         (mapc
          #'(lambda (tag-name)
              (setq tag-list (assoc tag-name tag-alist))
              (unless tag-list
                (add-to-list 'tag-alist (setq tag-list `(,tag-name))))
              (nconc tag-list (list attr-plist)))
          (plist-get attr-plist :tags)))
     file-attr-list)
    (unless (file-directory-p tag-base-dir)
      (mkdir tag-base-dir t))
    (owp/string-to-file
     (mustache-render
      (owp/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (owp/file-to-string (owp/get-template-file "container.mustache")))
      (ht ("header"
           (owp/render-header
            (ht ("page-title" (concat "Tag Index - " (owp/get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (owp/render-navigation-bar))
          ("content"
           (owp/render-content
            "tag-index.mustache"
            (ht ("tags"
                 (mapcar
                  #'(lambda (tag-list)
                      (ht ("tag-name" (car tag-list))
                          ("tag-uri" (owp/generate-tag-uri (car tag-list)))
                          ("count" (number-to-string (length (cdr tag-list))))))
                  tag-alist)))))
          ("footer"
           (owp/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
                ("creator-info" (owp/get-html-creator-string))
                ("email" (owp/confound-email-address (or user-mail-address
                                                        "Unknown Email"))))))))
     (concat tag-base-dir "index.html") 'html-mode)
    (mapc
     #'(lambda (tag-list)
         (setq tag-dir (file-name-as-directory
                        (concat tag-base-dir
                                (owp/encode-string-to-url (car tag-list)))))
         (unless (file-directory-p tag-dir)
           (mkdir tag-dir t))
         (owp/string-to-file
          (mustache-render
           (owp/get-cache-create
            :container-template
            (message "Read container.mustache from file")
            (owp/file-to-string (owp/get-template-file "container.mustache")))
           (ht ("header"
                (owp/render-header
                 (ht ("page-title" (concat "Tag: " (car tag-list)
                                           " - " (owp/get-config-option :site-main-title)))
                     ("author" (or user-full-name "Unknown Author")))))
               ("nav" (owp/render-navigation-bar))
               ("content"
                (owp/render-content
                 "tag.mustache"
                 (ht ("tag-name" (car tag-list))
                     ("posts"
                      (mapcar
                       #'(lambda (attr-plist)
                           (ht ("post-uri" (plist-get attr-plist :uri))
                               ("post-title" (plist-get attr-plist :title))))
                       (cdr tag-list))))))
               ("footer"
                (owp/render-footer
                 (ht ("show-meta" nil)
                     ("show-comment" nil)
                     ("author" (or user-full-name "Unknown Author"))
                     ("google-analytics" (owp/get-config-option :personal-google-analytics-id))
                     ("google-analytics-id" (owp/get-config-option :personal-google-analytics-id))
                     ("creator-info" (owp/get-html-creator-string))
                     ("email" (owp/confound-email-address (or user-mail-address
                                                             "Unknown Email"))))))))
          (concat tag-dir "index.html") 'html-mode))
     tag-alist)))

(defun owp/update-rss (file-attr-list pub-base-dir)
  "Update RSS. FILE-ATTR-LIST is the list of all file attribute property lists.
PUB-BASE-DIR is the root publication directory."
  (let ((last-10-posts
         (-take 10 (--sort (>= 0 (owp/compare-standard-date
                                  (owp/fix-timestamp-string
                                   (plist-get it :mod-date))
                                  (owp/fix-timestamp-string
                                   (plist-get other :mod-date))))
                           (--filter (not (or
                                           (string= (plist-get it :category)
                                                    "index")
                                           (string= (plist-get it :category)
                                                    "about")))
                                     file-attr-list)))))
    (owp/string-to-file
     (mustache-render
      owp/rss-template
      (ht ("title" (owp/get-config-option :site-main-title))
          ("link" (owp/get-site-domain))
          ("description" (owp/get-config-option :site-sub-title))
          ("date" (format-time-string "%a, %d %b %Y %T %Z"))
          ("items" (--map (ht ("item-title" (plist-get it :title))
                              ("item-link" (owp/get-full-url (plist-get it :uri)))
                              ("item-description" (plist-get it :description))
                              ("item-update-date" (plist-get it :mod-date)))
                          last-10-posts))))
     (concat (file-name-as-directory pub-base-dir) "rss.xml"))))


(provide 'owp-export)

;;; owp-export.el ends here
