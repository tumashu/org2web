;;; org-webpage.el --- static site generator based on org mode

;; Copyright (C)  2015 Feng Shu
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
(require 'owp-resource)
(require 'owp-export)
(require 'owp-web-server)
(require 'cl-lib)


(defconst org-webpage-version "0.1")

(defun owp/do-publication (&optional project-name publishing-directory)
  (interactive)
  (setq project-name
        (or project-name
            owp/default-project-name
            (completing-read "Which project do you want to publish? "
                             (delete-dups
                              (mapcar 'car owp/project-config-alist))
                             nil t nil nil owp/last-project-name)))
  (setq owp/current-project-name project-name
        owp/last-project-name project-name
        owp/item-cache nil)

  (let ((preparation-function
         (owp/get-config-option :preparation-function)))
    (when preparation-function
      (run-hooks 'preparation-function)))

  (owp/verify-configuration)
  (let* ((repo-dir (owp/get-repository-directory))
         (publish-root-dir
          (file-name-as-directory owp/temporary-directory))
         (export-dir
          (concat (file-name-as-directory
                   (concat publish-root-dir project-name))
                  "export/"))
         (history-dir
          (concat (file-name-as-directory
                   (concat publish-root-dir project-name))
                  "history/"))
         (publish-dir
          (or publishing-directory
              (concat (file-name-as-directory
                       (concat publish-root-dir project-name))
                      "publish/")
              (owp/get-publishing-directory)))
         (test-publish-dir
          (concat (file-name-as-directory
                   (concat publish-root-dir project-name))
                  "test/"))
         (upload-script
          (concat (file-name-as-directory
                   (concat publish-root-dir project-name))
                  "owp-upload-script.sh"))
         (remote (owp/get-config-option :remote))
         (site-domain (owp/get-site-domain))
         (repo-files (owp/directory-files-recursively repo-dir nil "\\.org$"))
         (changed-files `(:delete nil :update ,repo-files)))

    ;; (setq repo-files-history
    ;;       (let* ((buffer (url-retrieve-synchronously
    ;;                       (concat (file-name-as-directory site-domain) "ls-R.el")))
    ;;              (buffer-string (save-excursion
    ;;                               (set-buffer buffer)
    ;;                               (buffer-string))))
    ;;         (read buffer-string)))
    ;; (setq changed-files
    ;;       `(:delete nil :update ,(delq nil
    ;;                                    (mapcar
    ;;                                     #'(lambda (file)
    ;;                                         (let ((file-info (assoc (file-relative-name file repo-dir)
    ;;                                                                 repo-files-history)))
    ;;                                           (unless (and file-info
    ;;                                                        (string= (owp/get-modification-time file)
    ;;                                                                 (nth 1 file-info)))
    ;;                                             file)))
    ;;                                     repo-files))))

    (when (file-directory-p publish-root-dir)
      (delete-directory publish-root-dir t))

    (make-directory export-dir t)
    (make-directory history-dir t)
    (make-directory publish-dir t)
    (make-directory test-publish-dir t)

    (owp/prepare-theme-resources export-dir)
    (owp/publish-changes repo-files changed-files export-dir)
    (owp/generate-upload-script upload-script export-dir history-dir publish-dir remote)

    (if (and (file-exists-p upload-script)
             owp/terminal-emulater
             (executable-find "bash"))
        (shell-command (format "%s -e 'bash %s'"
                               owp/terminal-emulater
                               (expand-file-name upload-script)))
      (message "Can't run upload script file correctly!"))
    (setq owp/current-project-name nil)))

(defun owp/generate-upload-script (script-file export-dir history-dir publish-dir remote)
  "Generate a shell script file, which used to upload html files
generated by org-webpage to remote."
  (if (not (and script-file export-dir history-dir publish-dir remote))
      (message "Can't generate org-webpage upload script.")
    (with-temp-buffer
      (insert "#!/bin/bash\n\n")
      (insert "set -e\n\n")
      (insert "git_cmd=`which git`\n")
      (insert "export_dir='" (expand-file-name export-dir) "'\n")
      (insert "history_dir='" (expand-file-name history-dir) "'\n")
      (insert "publish_dir='" (expand-file-name publish-dir) "'\n")
      (insert "git_url='" (nth 1 remote) "'\n")
      (insert "git_branch='" (nth 2 remote) "'\n\n")
      (insert "

echo '###########################################################'
echo '##              Org-webpage upload script                ##'
echo '###########################################################'
read -p 'Run this script to upload org-webpage project? [y/n]' -n 1 -r

if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

echo '\n'
echo '###########################################################'
echo '##  Building history directory ...                       ##'
echo '###########################################################'
$git_cmd clone --depth=1 --branch $git_branch  $git_url $history_dir

echo '\n'
echo '###########################################################'
echo '##  Generate publish directory and git add/commit ...    ##'
echo '###########################################################'

cp -r $export_dir/*      $publish_dir
cp -r $history_dir/.git  $publish_dir

cd $publish_dir
git add --all .
git commit -m 'Update published html files, committed by org-webpage.'

echo '\n'
echo '###########################################################'
echo '## Run git push ...                                      ##'
echo '###########################################################'

read -p \"Push to: Remote: $git_url\n         Branch: $git_branch?  [y/n]\" -n 1 -r

if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

echo '\n'
git push origin $git_branch

echo 'Press any key to continue...'
read -s -n 1 any_key
exit 0")
      (when (file-writable-p script-file)
        (write-region (point-min)
                      (point-max) script-file)))))

(defun owp/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
1.  `:repository-directory': <required>
2.  `:site-domain': <required>
3.  `:personal-disqus-shortname': <optional>
4.  `:personal-duoshuo-shortname': <optional>
5.  `:site-main-title': [optional] (but customization recommanded)
6.  `:site-sub-title': [optional] (but customization recommanded)
7.  `:personal-github-link': [optional] (but customization recommended)
8.  `:personal-google-analytics-id': [optional] (but customization recommended)
9.  `:theme': [optional]"
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
           "# -*- coding: utf-8-unix; -*-
#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s

# #+URI:         %s
# #+KEYWORDS:    %s
# #+TAGS:        %s
# #+DESCRIPTION: %s

#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
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
           (if (string= description "")
               "<TODO: insert your description here>"
             description)
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
           org-export-with-timestamps)))

(defun owp/new-post (&optional project-name category filename insert-fallback-template)
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
          (f (read-string (format "Filename of \"%s\" project: " p) "new-post.org" p))
          (d (yes-or-no-p "Insert fallback template? ")))
     (list p c f d)))
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
    (if (and (not insert-fallback-template)
             (called-interactively-p 'any))
        (call-interactively 'owp/insert-options-template)
      (owp/insert-options-template "<Insert Your Title Here>"
                                   (format "/%s/%%y/%%m/%%d/%%t/ Or /%s/%%t/"
                                           category category)
                                   "keyword1, keyword2, keyword3"
                                   "tag1, tag2, tag3"
                                   "<Add description here>"))
    (save-buffer))
  (setq owp/current-project-name nil))


(provide 'org-webpage)

;;; org-webpage.el ends here
