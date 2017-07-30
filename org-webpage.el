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

(defcustom owp/default-project-name nil)
(defcustom owp/temporary-directory "~/.emacs.d/owp-temp.d/")
(defcustom owp/terminal-emulater)
(defcustom owp/uploader-config-alist)
(defcustom owp/project-config-alist nil)
(defcustom owp/get-config-option-function)
(defconst owp/temp-buffer-name "*Org Page Output*")
(defconst owp/load-directory)
(defvar owp/category-config-alist)

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
  `(:repository-directory
    nil
    :ignore ("-pkg\\.org$" "-autoloads\\.org" "#\\..*")
    :publishing-directory nil
    :remote nil
    :site-domain nil
    :old-site-domain nil
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
    :category-ignore-list ("themes" "assets" "uploaders")
    :summary (("tags" :tags))
    :confound-email t
    :force-absolute-url t
    :preparation-function nil
    :get-title-function owp/get-title
    :retrieve-category-function owp/get-file-category
    :org-export-function owp/default-org-export
    :web-server-docroot "~/.emacs.d/org-webpage-server/default"
    :web-server-port 9876
    :el2org-doc-sources nil
    :el2org-readme-sources nil
    :el2org-index-sources nil
    :el2org-doc-tags ("README" "devel" "doc" "code")
    :el2org-readme-tags ("README")
    :el2org-index-tags ("README")
    :html-creator-string ,(format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
(<a href=\"http://orgmode.org\">Org mode</a> %s)"
(format "%s.x" emacs-major-version)
(if (fboundp 'org-version)
    (replace-regexp-in-string "\\..*" ".x" (org-version))
  "Unknown Version"))
"If User don't set an option, org-webpage will use fallback value of this option."))



(defun owp/add-project-config (project-config))
(defun owp/select-project-name (prompt &optional project-name))
(defun owp/do-publication (&optional project-name publishing-directory job-number update-top-n))
(defun owp/new-post (&optional project-name category filename insert-fallback-template))

;; * Footer
(provide 'org-webpage)

;;; org-webpage.el ends here
