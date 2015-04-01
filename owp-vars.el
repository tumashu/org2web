;;; owp-vars.el --- Variable configurations required by org-webpage

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

;; owp-vars.el contains almost all variable definitions and configurations.

;;; Code:

(require 'ox)
(require 'ht)


(defgroup org-webpage nil
  "Options for generating static pages using org-webpage."
  :tag "Org static page generator"
  :group 'org)

(defcustom owp/default-project-name nil
  "If set, `owp/do-publication' will directly publish this project
and `owp/new-post' will directly add new post to this project."
  :group 'org-webpage
  :type 'string)

(defcustom owp/project-config-alist nil
  "Association list to control org-webpage publishing behavior.

Each element of the alist is a org-webpage 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the publishing process.

  \(:property value :property value ... )

Most properties are optional, but some should always be set:

  `:repository-directory'

The git repository directory, where org files stored on branch
`:repository-org-branch', and generated html files stored on branch
`:repository-html-branch'.

  `:site-domain'

The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned.

  `:site-main-title'

The main title of entire site.

  `:site-sub-title'

The subtitle of entire site.

  `:preparation-function'

Function to be called before publishing this project.  This may also
be a list of functions.


  `:repository-org-branch'

The branch where org files stored on, it is within repository presented by
`:repository-directory'.

  `:repository-html-branch'

The branch where generated html files stored on, it is within repository
presented by `:repository-directory'.

  `:theme-root-directory'

The root directory list that stores themes for page rendering. By default, it
points to the directory `themes' in org-webpage installation directory.

  `:theme'

The theme used for page generation.

  `:personal-github-link'

The personal github link.

  `:personal-avatar'

The link to an avatar image.

  `:personal-disqus-shortname'

The personal disqus shortname.

  `:personal-duoshuo-shortname'

The personal duoshuo shortname.

  `:personal-google-analytics-id'

Personal google analytics id.

  `:confound-email'

Determine whether email addresses should be confounded or not.

  `:default-category'

If org fils don't set category, default category will be used.

  `:category-ignore-list'

Ignore subdirs/categories for navigation

  `:get-title-function'

A function used to retrieve an org file's Title, it has no parameter and
run org file buffer.

  `:retrieve-category-function'

A function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories.

   `:org-export-function'

Set the default function by which org-webpage export org file to html.

  `:html-creator-string'

Information about the creator of the HTML document.

  `:repo-files-function'

The function used to get all org files exported.

  `:web-server-docroot'

org-webpage can start a web server to test publish, this
set the server document root.

  `:web-server-port'

org-webpage can start a web server to test publish, this
set the server port.

You can see fallback value of above option in `owp/config-fallback'"
  :group 'org-webpage
  :type 'alist)

(defcustom owp/get-config-option-function
  'owp/get-config-option-from-alist
  "The function used to get config option."
  :group 'org-webpage
  :type 'function)

(defconst owp/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-webpage.")

(defconst owp/load-directory
  (cond
   (load-file-name (file-name-directory load-file-name))
   ((symbol-file 'owp/temp-buffer-name)
    (file-name-directory (symbol-file 'owp/temp-buffer-name)))
   ((string= (file-name-nondirectory buffer-file-name) "owp-vars.el")
    (file-name-directory buffer-file-name))
   (t nil))
  "The directory where org-webpage is loaded from.")

(defvar owp/category-config-alist
  '(("blog" ;; this is the default configuration
     :show-meta t
     :show-comment t
     :uri-generator owp/generate-uri
     :uri-template "/blog/%y/%m/%d/%t/"
     :sort-by :date     ;; how to sort the posts
     :category-index t) ;; generate category index or not
    ("index"
     :show-meta nil
     :show-comment nil
     :uri-generator owp/generate-uri
     :uri-template "/"
     :sort-by :date
     :category-index nil)
    ("about"
     :show-meta nil
     :show-comment nil
     :uri-generator owp/generate-uri
     :uri-template "/about/"
     :sort-by :date
     :category-index nil))
  "Configurations for different categories, can and should be customized.")

(defvar owp/current-project-name nil)

(defvar owp/item-cache nil
  "The cache for general purpose.")

(defconst owp/rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>{{title}}</title>
    <link>{{link}}</link>
    <description>{{description}}</description>
    <pubDate>{{date}}</pubDate>
    <lastBuildDate>{{date}}</lastBuildDate>
    <docs>http://www.rssboard.org/rss-specification</docs>
    <generator>org-webpage static site generator \
(https://github.com/tumashu/org-webpage)</generator>
{{#items}}
<item>
<title>{{item-title}}</title>
<link>{{item-link}}</link>
<description>{{item-description}}</description>
<pubDate>{{item-update-date}}</pubDate>
<guid>{{item-link}}</guid>
</item>
{{/items}}
</channel>
</rss>"
  "Template for RSS rendering.")

(defvar owp/config-fallback
      `(:repository-directory nil
        :site-domain nil
        :site-main-title "org-webpage"
        :site-sub-title "static site generator"
        :repository-org-branch "source"
        :repository-html-branch "master"
        :theme-root-directory nil
        :theme (default)
        :personal-github-link "https://github.com/tumashu/org-webpage"
        :personal-avatar nil
        :personal-disqus-shortname nil
        :personal-duoshuo-shortname nil
        :personal-google-analytics-id nil
        :default-category "blog"
        :category-ignore-list ("themes" "assets")
        :confound-email t
        :preparation-function nil
        :get-title-function owp/get-title
        :retrieve-category-function owp/get-file-category
        :repo-files-function owp/git-all-files
        :org-export-function owp/default-org-export
        :web-server-docroot "~/.emacs.d/org-webpage-server/default"
        :web-server-port 9876
        :html-creator-string ,(format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
(<a href=\"http://orgmode.org\">Org mode</a> %s)"
(format "%s.x" emacs-major-version)
(if (fboundp 'org-version)
    (replace-regexp-in-string "\\..*" ".x" (org-version))
  "Unknown Version"))
"If User don't set an option, org-webpage will use fallback value of this option."))


(provide 'owp-vars)

;;; owp-vars.el ends here
