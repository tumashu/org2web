;;; org-webpage.el --- static site generator based on org mode

;; Copyright (C)  2005 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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

;; org-webpage is a static site generator based on org mode.

;; 1. Sources:   https://github.com/tumashu/org-webpage
;; 2. Documents: http://tumashu.github.io/org-webpage

;; org-webpage is a fork of Kelvin H's org-page (https://github.com/kelvinh/org-page),
;; and provides similar features as org-page, the main differents is as follow:

;; 1. org-page focus on personal blog while org-webpage is main used to
;;    generate small project website.
;; 2. org-page use many customizable variables to configure org-page
;;    while org-website use an `org-publish-project-alist' style
;;    alist to adjust org-website's behaver.

;;    Managing multi-site configs in an emacs session with org-website is more
;;    simple than with org-page.
;; 3. org-website can deal with "increment" or "inherit" themes.
;;
;;    A "increment" theme is a mod theme which only include changed template,
;;    css and other files, the files same with base theme doesn't include.

;;    org-webpage autosearch the same files from base theme when use "increment"
;;    theme.
;; 4. org-website include a tiny emacs web server, which can be used to test publish.


;;; Code:

(require 'ox)
(require 'ht)
(require 'owp-util)
(require 'owp-vars)
(require 'owp-config)
(require 'owp-git)
(require 'owp-resource)
(require 'owp-export)
(require 'owp-web-server)
(require 'cl-lib)


(defconst org-webpage-version "0.1")

(defun owp/do-publication (&optional project-name
                                     force-all
                                     base-git-commit pub-base-dir
                                     auto-commit auto-push)
  "The main entrance of org-webpage. The entire procedure is:
1) verify configuration
2) read changed files on \"org branch\" of \"repository directory\",
   the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
      will be published.
   2. if FORCE-ALL is nil, the changed files will be obtained based on
      BASE-GIT-COMMIT
   3. if BASE-GIT-COMMIT is nil or omitted, the changed files will be obtained
      based on previous commit
3) publish org files to html, if PUB-BASE-DIR is specified, use that directory
   to store the generated html files, otherwise html files will be stored on \"html-branch\"
   of \"repository directory\".
4) if PUB-BASE-DIR is nil, and AUTO-COMMIT is non-nil, then the changes stored
   on \"html-branch\" will be automatically committed, but be careful, this feature is
   NOT recommended, and a manual commit is much better
5) if PUB-BASE-DIR is nil, AUTO-COMMIT is non-nil, and AUTO-PUSH is non-nil,
then the \"html-branch\"  will be pushed to remote repo."
  (interactive
   (let* ((j (or owp/default-project-name
                 (completing-read "Which project do you want to publish? "
                                  (delete-dups
                                   (mapcar 'car owp/project-config-alist))
                                  nil t nil nil owp/last-project-name)))
          (f (y-or-n-p (format "Publish all org files of \"%s\" project? " j)))
          (b (unless f (read-string "Base git commit: " "HEAD~1")))
          (p (progn (setq owp/current-project-name j)
                    (setq owp/last-project-name j)
                    (when (y-or-n-p
                           "Publish to:  [Yes] Web server docroot, [No] Original repo. ")
                      (expand-file-name (owp/get-config-option :web-server-docroot)))))
          (a (when (and (not p))
               (y-or-n-p "Auto commit to repo? ")))
          (u (when (and a (not p))
               (y-or-n-p "Auto push to remote repo? "))))
     (list j f b p a u)))

  (let ((preparation-function
         (owp/get-config-option :preparation-function)))
    (when preparation-function
      (run-hooks 'preparation-function)))

  (owp/verify-configuration)
  (setq owp/item-cache nil)
  (let* ((repo-dir (owp/get-repository-directory))
         (org-branch (owp/get-config-option :repository-org-branch))
         (html-branch (owp/get-config-option :repository-html-branch))
         (repo-files-function (owp/get-config-option :repo-files-function))
         (addition-files-function (owp/get-config-option :addition-files-function))
         (orig-branch (owp/git-branch-name repo-dir))
         (to-repo (not (stringp pub-base-dir)))
         (store-dir (if to-repo "~/.owp-tmp/" pub-base-dir)) ; TODO customization
         (owp/publish-to-repository to-repo)
         repo-files addition-files changed-files remote-repos)
    (owp/git-change-branch repo-dir org-branch)
    (owp/prepare-theme-resources store-dir)
    (setq repo-files
          (when (functionp repo-files-function)
            (funcall repo-files-function repo-dir)))
    (setq addition-files
          (when (functionp addition-files-function)
            (funcall addition-files-function repo-dir)))
    (setq changed-files (if force-all
                            `(:update ,repo-files :delete nil)
                          (owp/git-files-changed repo-dir (or base-git-commit "HEAD~1"))))
    (owp/publish-changes repo-files addition-files changed-files store-dir)
    (when to-repo
      (owp/git-change-branch repo-dir html-branch)
      (copy-directory store-dir repo-dir t t t)
      (delete-directory store-dir t))
    (when (and to-repo auto-commit)
      (owp/git-commit-changes repo-dir (concat "Update published html files, "
                                               "committed by org-webpage."))
      (when auto-push
        (setq remote-repos (owp/git-remote-name repo-dir))
        (if (not remote-repos)
            (message "No valid remote repository found.")
          (let (repo)
            (if (> (length remote-repos) 1)
                (setq repo (read-string
                            (format "Which repo to push %s: "
                                    (prin1-to-string remote-repos))
                            (car remote-repos)))
              (setq repo (car remote-repos)))
            (if (not (member repo remote-repos))
                (message "Invalid remote repository '%s'." repo)
              (owp/git-push-remote repo-dir
                                   repo
                                   html-branch)))))
      (owp/git-change-branch repo-dir orig-branch))
    (if to-repo
        (message "Publication finished: on branch '%s' of repository '%s'."
                 html-branch repo-dir)
      (message "Publication finished, output directory: %s." pub-base-dir)
      (when (called-interactively-p 'any)
        (owp/web-server-browse)))
    (setq owp/current-project-name nil)))

(defun owp/new-repository (repo-dir)
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by org-webpage."
  (interactive
   (list (read-directory-name
          "Specify a directory to become the repository: " nil nil nil)))
  (owp/git-init-repo repo-dir)
  (owp/generate-readme repo-dir)
  (owp/git-commit-changes repo-dir "initial commit")
  (owp/git-new-branch repo-dir (owp/get-config-option :repository-org-branch))
  (owp/generate-index repo-dir)
  (owp/git-commit-changes repo-dir "add source index.org")
  (owp/generate-about repo-dir)
  (owp/git-commit-changes repo-dir "add source about.org"))

(defun owp/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
1.  `:repository-directory': <required>
2.  `:site-domain': <required>
3.  `:personal-disqus-shortname': <optional>
4.  `:personal-duoshuo-shortname': <optional>
5.  `:repository-org-branch': [optional] (but customization recommended)
6.  `:repository-html-branch': [optional] (but customization recommended)
7.  `:site-main-title': [optional] (but customization recommanded)
8.  `:site-sub-title': [optional] (but customization recommanded)
9.  `:personal-github-link': [optional] (but customization recommended)
10. `:personal-google-analytics-id': [optional] (but customization recommended)
11. `:theme': [optional]"
  (unless (member owp/current-project-name
                  (mapcar 'car owp/project-config-alist))
    (error "Can't find project: \"%s\"" owp/current-project-name))
  (let ((repo-dir (owp/get-repository-directory))
        (site-domain (owp/get-site-domain)))
    (unless (and repo-dir (file-directory-p repo-dir))
      (error "Repository directory is not properly configured."))
    (unless site-domain
      (error "Site domain is not properly configured."))))

(defun owp/generate-readme (save-dir)
  "Generate README for `owp/new-repository'. SAVE-DIR is the directory where to
save generated README."
  (owp/string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and org-webpage."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by org-webpage \"owp/new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by org-webpage.")
   (expand-file-name "README" save-dir)))

(defun owp/generate-index (save-dir)
  "Generate index.org for `owp/new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (owp/string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun owp/generate-about (save-dir)
  "Generate about.org for `owp/new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (owp/string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by org-webpage.")
   (expand-file-name "about.org" save-dir)))

(defun owp/insert-options-template (&optional title uri
                                              keywords tags description)
  "Insert a template into current buffer with information for exporting.

TITLE: the title of this post
URI: the uri of this post, usually looks like: /2013/12/27/the-post-title,
the following parameters could be used:
    %y: to represent the year of creation date
    %m: to represent the month of creation date
    %d: to represent the day of creation date
KEYWORDS: the keywords of this post, used by search engine
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed

Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid."
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/%c/%y/%m/%d/%t"
                                 `((?c . ,(owp/get-config-option :default-category))
                                   (?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(owp/encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "")
               "<TODO: insert your description here>"
             description))))

(defun owp/new-post (&optional project-name category filename)
  "Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive
   (let* ((p (or owp/default-project-name
                 (completing-read "Which project do you want post? "
                                  (delete-dups
                                   (mapcar 'car owp/project-config-alist))
                                  nil t nil nil owp/last-project-name)))
          (c (read-string (format "Category of \"%s\" project: " p)
                          (progn (setq owp/current-project-name p)
                                 (setq owp/last-project-name p)
                                 (owp/get-config-option :default-category))))
          (f (read-string (format "Filename of \"%s\" project: " p) "new-post.org" p)))
     (list p c f)))
  (if (string= category "")
      (setq category (owp/get-config-option :default-category)))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (owp/string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((repo-dir (owp/get-repository-directory))
         (dir (concat (file-name-as-directory repo-dir)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (if (called-interactively-p 'any)
        (call-interactively 'owp/insert-options-template)
      (owp/insert-options-template "<Insert Your Title Here>"
                                   "/%y/%m/%d/%t/"
                                   "add, keywords, here"
                                   "add, tags, here"
                                   "add description here"))
    (save-buffer))
  (setq owp/current-project-name nil))


(provide 'org-webpage)

;;; org-webpage.el ends here
