;;; owp-vars.el --- Variable configurations required by org-webpage

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

(defcustom owp/temporary-directory "~/.emacs.d/owp-temp.d/"
  "Temporary directory used by org-webpage."
  :group 'org-webpage
  :type 'string)

(defcustom owp/terminal-emulater
  (or (executable-find "x-terminal-emulator")
      (executable-find "gnome-terminal")
      (executable-find "konsole")
      (executable-find "rxvt-unicode")
      (executable-find "rxvt")
      (executable-find "xterm"))
  "Terminal emulater used by org-webpage update script."
  :group 'org-webpage
  :type 'string)

(defcustom owp/project-config-alist nil
  "Association list to control org-webpage publishing behavior.

Each element of the alist is a org-webpage project.  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the publishing process.

  \(:property value :property value ... )

Most properties are optional, but some should always be set:


  `:repository-directory'

The repository directory, which containing publishing org files.
1. Type: string
2. Example1: \"~/.emacs.d/projects/tumashu.github.com/\"


  `:ignore'

Ignore files in repository directory when publish.
1. Type: regexp list
2. Example1: (\"regexp1\" \"regexp2\")


  `:publishing-directory'

Directory (possibly remote) where html output files will be
published.


  `:remote'

A remote, published html file by org-webpage will be push/upload to.
1. Type: list
2. Example1: (git \"https://github.com/tumashu/org-webpage.git\" \"gh-pages\")


  `:site-domain'

The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned.
1. Type: string
2. Example1: \"http://tumashu.github.com\"


  `:site-main-title'

The main title of entire site.
1. Type: string
2. Example1: \"Tumashu's website\"


  `:site-sub-title'

The subtitle of entire site.
1. Type: string
2. Example1: \"======> My personal blog site.\"


  `:preparation-function'

Function to be called before publishing this project.  This may also
be a list of functions.
1. Type: function
2. Example: eh-convert-el-to-org


  `:theme-root-directory'

The root directory list that stores themes for page rendering. By default, it
points to the directory `themes' in org-webpage installation directory.
1. Type: list
2. Example1: (\"/path/to/dir1\" \"/path/to/dir2\" \"/path/to/dir3\" ...)
3. Example2: nil

When set this option to `nil', org-webpage will find two paths as fallback:
1. <Directory which contain org-webpage.el>/themes
2. <Your project repository directory>/themes


  `:theme'

The theme used for page generation.
1. Type: list
2. Example1: (worg killjs)
3. Example2: nil

When set this option to `nil', default theme will be used.


  `:source-browse-url'

The personal github link.
1. Type: list
2. Example1: (\"GitHub\" \"https://github.com/tumashu/org-webpage\")


  `:personal-avatar'

The link to an avatar image.
1. Type: string
2. Example1: \"/media/img/horse.jpg\"
2. Example2: \"http://tumashu.github.com/org-webpage/media/img/horse.jpg\"


  `:personal-disqus-shortname'

The personal disqus shortname.
1. Type: string
2. Example1: \"my-disqus-name\"


  `:personal-duoshuo-shortname'

The personal duoshuo shortname.
1. Type: string
2. Example1: \"my-duoshuo-name\"


  `:personal-google-analytics-id'

Personal google analytics id.
The personal duoshuo shortname.
1. Type: string
2. Example1: \"my-google-analytics-id\"


  `:confound-email'

Determine whether email addresses should be confounded or not.
1. Type: boolean
2. Example1: t

When set this option to `t', \"myname@163.com\" will be converted to \"myname <at> 163 <dot> com\"


  `:force-absolute-url'

Force convert relative url to absolute url in html files by append site domain.
1. Type: boolean
2. Example1: t

When set this option to `t', all url like \"/path/to/file.html\" will be
converted to \"http://yourdomain.com/path/to/file.html\".


  `:default-category'

If org fils don't set category, default category will be used.
1. Type: string
2. Example1: \"blog\"
3. Example2: \"wiki\"
4. Example3: \"documents\"

  `:about'

About page of org-website.
1. Type: list
2. Example1: (\"About\" \"/about/\")


  `:rss'

RSS page of org-website.
1. Type: list
2. Example1: (\"RSS\" \"/rss.xml\")


  `:summary'

A summary is a statistic page, Which can be used show pages
based on \"tags\" , \"data\" , \"author\" and so on.
it is similar Micorsoft Excel pivot table feature.
1. Type: alist
2. Example1: ((\"tags\" :tags) (\"date\" :date) (\"authors\" :authors))
2. Example2: ((\"按标签分类\" :tags) (\"按时间分类\" :date) (\"按作者分类\" :authors))


  `:category-ignore-list'

Ignore subdirs/categories for navigation.
1. Type: list
2. Example1: (\"themes\" \"assets\")

Names in this list will not showed in webpage navbar.


  `:get-title-function'

A function used to retrieve an org file's Title, it has no parameter and
run org file buffer.
1. Type: function
2. Example1: owp/get-title


  `:retrieve-category-function'

A function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories.
1. Type: function
2. Example1: owp/get-file-category


   `:org-export-function'

Set the default function by which org-webpage export org file to html.
1. Type: function
2. Example1: owp/default-org-export


  `:html-creator-string'

Information about the creator of the HTML document.
1. Type: string
2. Example1: \"This is an example creator string\"

  `:web-server-docroot'

org-webpage can start a web server to test publish, this
set the server document root.
1. Type: string
2. Example1: \"~/.emacs.d/org-website-server/org-webpage/\"


  `:web-server-port'

org-webpage can start a web server to test publish, this
set the server port.
1. Type: number
2. Example1: 9876


  `:lentic-doc-sources'

Emacs-lisp files from which org files will be generated with the
help of lentic:
1. Type: regexp-list
2. Example: (\"org-webpage.el\" \"owp-*.el\")


  `:lentic-doc-tags'

The content with these tags will be considered when generate org files
from `:lentic-doc-source'.
1. Type: list
2. Example: (\"tag1\" \"tag2\" \"tag3\")



  `:lentic-readme-sources'

Emacs-lisp or org file from which README.md will be generated with the
help of lentic:
1. Type: file name list
2. Example: (\"org-webpage.el\")

NOTE: At the moment, *only* use the *first* element of list, NEED improve.


  `:lentic-readme-tags'

The content with these tags will be considered when generate README.md
from `:lentic-readme-source'.
1. Type: list
2. Example: (\"tag1\" \"tag2\" \"tag3\")



  `:lentic-index-sources'

Emacs-lisp or org file from which index.org will be generated with the
help of lentic:
1. Type: file name list
2. Example: (\"org-webpage.el\")

NOTE: At the moment, *only* use the *first* element of list, NEED improve.



  `:lentic-index-tags'

The content with these tags will be considered when generate index.org
from `:lentic-index-source'.
1. Type: list
2. Example: (\"tag1\" \"tag2\" \"tag3\")


You can see fallback value of above option in `owp/config-fallback'

Note: Advanced user can use (:eval form) to config *All* org-webpage config options,
for example, set `:repository-directory' to:

 (:eval (concat \"~/.emacs.d/project/\" \"tumashu.github.com/\"))

This feature is very useful in certain case."
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
  '(("blog"
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
(defvar owp/last-project-name nil)
(defvar owp/buffer-name " *org-webpage buffer*")

(defvar owp/publish-to-repository nil)
(defvar owp/always-use-relative-url nil
  "Always use relative url in exported html files, this is useful for
test publish.")

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
        :ignore ("-pkg\\.org$" "-autoloads\\.org" "#\\..*")
        :publishing-directory nil
        :remote nil
        :site-domain nil
        :site-main-title "org-webpage"
        :site-sub-title "static site generator"
        :theme-root-directory nil
        :theme (default)
        :source-browse-url nil
        :personal-avatar nil
        :personal-disqus-shortname nil
        :personal-duoshuo-shortname nil
        :personal-google-analytics-id nil
        :default-category "blog"
        :about ("About" "/about/")
        :rss ("RSS" "/rss.xml")
        :category-ignore-list ("themes" "assets" "upload-scripts")
        :summary (("tags" :tags))
        :confound-email t
        :force-absolute-url t
        :preparation-function nil
        :get-title-function owp/get-title
        :retrieve-category-function owp/get-file-category
        :org-export-function owp/default-org-export
        :web-server-docroot "~/.emacs.d/org-webpage-server/default"
        :web-server-port 9876
        :lentic-doc-sources nil
        :lentic-readme-sources nil
        :lentic-index-sources nil
        :lentic-doc-tags ("README" "devel" "doc" "code")
        :lentic-readme-tags ("README")
        :lentic-index-tags ("README")
        :html-creator-string ,(format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
(<a href=\"http://orgmode.org\">Org mode</a> %s)"
(format "%s.x" emacs-major-version)
(if (fboundp 'org-version)
    (replace-regexp-in-string "\\..*" ".x" (org-version))
  "Unknown Version"))
"If User don't set an option, org-webpage will use fallback value of this option."))


(provide 'owp-vars)

;;; owp-vars.el ends here
