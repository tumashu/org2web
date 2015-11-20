;;; org-webpage.el --- static site generator based on org mode

;; * Header
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

;; * org-webpage README                                                 :README:

;; org-webpage is a static site generator based on [[http://orgmode.org/][org-mode]],
;; which code derived from Kelvin H's [[https://github.com/kelvinh/org-page][org-page]].

;; The main differents of two projects are as follow:

;; 1. org-webpage's core *don't hard code git*, its process is like below:
;;    #+BEGIN_EXAMPLE
;;    [ Org files ] --( Export )--> [ Html files ] -------
;;          \                                             \
;;           \--------(Generate)--> [ Upload bash script] ---> ( Git repos )--\
;;                                         \                                   \
;;                                          \------------------------------------( Upload )---> Remote

;;    #+END_EXAMPLE

;; 2. org-webpage's default config is `org-publish-project-alist' style alist,
;;    which can manage multi-site configs in an emacs session easily.
;; 3. org-website find theme-files from a *themes-list* in sequence and same theme-file
;;    first found will be used. User can set *fallback theme* with the help of this feature.
;; 4. org-website include a tiny emacs web server, which can be used to test publish.
;; 5. ...

;; ** Installation

;; org-webpage is now available from the famous emacs package repo [[http://melpa.milkbox.net/][melpa]]
;; so the recommended way is to install it through emacs package
;; management system. For more info about installation, please see
;; *tips.org* in the "doc" folder.

;; ** Configuration
;; org-webpage use variable `owp/project-config-alist' to store all projects's configures, user
;; can add a project with the help of `add-to-list' function, but the easiest way is
;; using `owp/add-project-config' function.

;; The follow code is [[http://tumashu.github.com][my website]]'s [[https://github.com/tumashu/tumashu.github.com/blob/source/eh-website.el][config]],
;; you can adjust and paste it to your =.emacs= file:

;; #+BEGIN_EXAMPLE
;; the following is only needed if you install org-page manually
;; (add-to-list 'load-path "path/to/org-webpage")

;; (require 'org-webpage)

;; (owp/add-project-config
;;  '("tumashu.github.com"
;;    :repository-directory "~/project/emacs-packages/tumashu.github.com"
;;    :remote (git "https://github.com/tumashu/tumashu.github.com.git" "master")
;;    :site-domain "http://tumashu.github.com/"
;;    :site-main-title "Tumashu 的个人小站"
;;    :site-sub-title "(九天十地，太上忘情！！！)"
;;    :theme (worg)
;;    :source-browse-url ("Github" "https://github.com/tumashu/tumashu.github.com")
;;    :personal-avatar "/media/img/horse.jpg"
;;    :personal-duoshuo-shortname "tumashu-website"
;;    :web-server-port 7654))
;; #+END_EXAMPLE

;; [[https://github.com/tumashu/chinese-pyim][Chinese-pyim]] 's org-webpage [[https://github.com/tumashu/chinese-pyim/blob/master/chinese-pyim-devtools.el][config]] is a more complex example.

;; You can find more config options and theirs default values by commands:

;; #+BEGIN_EXAMPLE
;; C-h v owp/project-config-alist
;; C-h v owp/config-fallback
;; #+END_EXAMPLE

;; ** Publication

;; #+BEGIN_EXAMPLE
;; M-x owp/do-publication
;; #+END_EXAMPLE

;; ** Dependencies

;; 1. [[http://www.gnu.org/software/emacs/][emacs]]: this is an "of-course" dependency
;; 2. [[http://orgmode.org/][org mode]]: v8.0 is required, please use =M-x org-version <RET>= to make sure you org mode version is not less than 8.0
;; 3. [[http://www.gnu.org/software/bash/][bash]]: the GNU Project's shell
;; 4. [[http://git-scm.com][git]]: a free and open source version control system
;; 5. [[https://github.com/Wilfred/mustache.el][mustache.el]]: a mustache templating library for Emacs
;; 6. [[http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi][htmlize.el]]: a library for syntax highlighting (usually this library is shipped with emacs)
;; 7. [[https://github.com/magnars/dash.el][dash.el]]: a modern list library for Emacs
;; 8. [[https://github.com/Wilfred/ht.el][ht.el]]: a modern hash-table library for Emacs
;; 9. [[https://github.com/eschulte/emacs-web-server][web-server]]: a web server library for Emacs

;; ** Known issues

;; 1. Currently the deletion change handler has not been implemented so
;;    if you deleted some org sources, you may have to manually delete
;;    corresponding generated html files.
;; 2. URI path change detection is not available. That is, if you make a
;;    post with the URI "/blog/2013/03/25/the-old-post-name" and then
;;    change this value in your org source, org-webpage would be unable to
;;    detect that this has happened. it will only publish a new html
;;    file for you so you need to delete the old html file related to
;;    the old URI manually.


;;; Code:

;; * 代码说明                                                             :code:

;; #+BEGIN_SRC emacs-lisp

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

(defun owp/add-project-config (project-config)
  "Add `project-config' to `owp/project-config-alist'"
  (if (listp project-config)
      (let ((project-name (car project-config)))
        (when (stringp project-name)
          (setq owp/project-config-alist
                (remove (assoc project-name owp/project-config-alist)
                        owp/project-config-alist)))
        (add-to-list 'owp/project-config-alist project-config))
    (message "Invalid project config!")))

(defun owp/select-project-name (prompt &optional project-name)
  "Let user select a project then return its name."
  (setq owp/current-project-name nil)
  (setq project-name
        (or project-name
            owp/default-project-name
            (completing-read prompt
                             (delete-dups
                              (mapcar 'car owp/project-config-alist))
                             nil t nil nil owp/last-project-name)))
  (setq owp/current-project-name project-name
        owp/last-project-name project-name)
  project-name)

(defun owp/do-publication (&optional project-name publishing-directory job-number update-top-n)
  (interactive)
  (setq project-name (owp/select-project-name "Which project do you want to publish? " project-name))
  (setq owp/item-cache nil)
  (let ((preparation-function
         (owp/get-config-option :preparation-function)))
    (when preparation-function
      (run-hooks 'preparation-function)))

  (owp/verify-configuration)
  (let* ((jobs '((1 . "Full publish")
                 (2 . "Partial publish")
                 (3 . "Test full publish")
                 (4 . "Test partial publish")))
         (job-used (completing-read
                    "Which job do you want to active: "
                    (mapcar #'cdr jobs)))
         (job-number (car (rassoc job-used jobs)))
         (test-publish (or (= job-number 3)
                           (= job-number 4)))
         (partial-update (or (= job-number 2)
                             (= job-number 4)))
         (repo-dir (owp/get-repository-directory))
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
         (repo-files
          (sort (owp/remove-matched-items
                 (owp/directory-files-recursively repo-dir "\\.org$")
                 (owp/get-config-option :ignore))
                #'(lambda (a b)
                    (time-less-p
                     (cl-sixth (file-attributes b))
                     (cl-sixth (file-attributes a))))))
         (length-repo-files (length repo-files))
         (update-top-n
          (cond ((and partial-update (numberp update-top-n)) update-top-n)
                (partial-update
                 (let ((max-mini-window-height 0.9)
                       (max-line (min length-repo-files 20)))
                   (read-number
                    (concat (let ((i 1))
                              (mapconcat
                               #'(lambda (file)
                                   (prog1 (format "%2s. %s"
                                                  (number-to-string i)
                                                  (file-relative-name file repo-dir))
                                     (setq i (+ i 1))))
                               (append (cl-subseq repo-files 0 max-line)
                                       (when (> length-repo-files max-line)
                                         '("## Hide others ... ##")))
                               "\n"))
                            "\n\nOrg-webpage will update TOP (N) org-files, Please type N: "))))))
         (changed-files (if (numberp update-top-n)
                            (cl-subseq repo-files 0 (min update-top-n length-repo-files))
                          repo-files)))

    (when (file-directory-p publish-root-dir)
      (delete-directory publish-root-dir t))

    (make-directory export-dir t)
    (make-directory history-dir t)
    (make-directory publish-dir t)
    (make-directory test-publish-dir t)

    (if test-publish
        (let ((owp/always-use-relative-url t)) ; Local test website, can't use absolute path.
          (owp/prepare-theme-resources test-publish-dir)
          (owp/publish-changes repo-files changed-files test-publish-dir)
          (owp/web-server-browse test-publish-dir
                                 (or (owp/get-config-option :web-server-port)
                                     (+ (* (random 9) 1000)
                                        (* (random 9) 100)
                                        (* (random 9) 10)
                                        (* (random 9) 1)))))
      (owp/prepare-theme-resources export-dir)
      (owp/publish-changes repo-files changed-files export-dir)
      (owp/generate-upload-script upload-script
                                  export-dir history-dir publish-dir
                                  remote
                                  partial-update)
      (if (and (file-exists-p upload-script)
               (executable-find "bash"))
          (cond
           ((string-equal system-type "windows-nt")
            (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" upload-script t t)))
           ((and (string-equal system-type "gnu/linux")
                 owp/terminal-emulater)
            (shell-command (format "%s -e 'bash %s'"
                                   owp/terminal-emulater
                                   (expand-file-name upload-script)))))
        (message "Can't run upload script file!

User should install 'bash' and 'git' correctly:
1. In Linux/Unix system, user can install 'bash' and 'git' with package manager.
2. In Window system, user can install 'msysgit',
   then add '<INSTALL-PATH>/bin' to envirment variable '$PATH'")))))

(defun owp/generate-upload-script (script-file export-dir history-dir publish-dir remote &optional partial-update)
  "Generate a shell script file, which used to upload html files
generated by org-webpage to remote."
  (if (not (and script-file export-dir history-dir publish-dir remote))
      (message "Can't generate org-webpage upload script.")
    (owp/string-to-file
     (mustache-render
      (owp/file-to-string (owp/get-upload-script-file
                           (concat (symbol-name (nth 0 remote)) ".mustache")))
      (ht ("export-dir" (expand-file-name export-dir))
          ("history-dir" (expand-file-name history-dir))
          ("publish-dir" (expand-file-name publish-dir))
          ("remote-url" (nth 1 remote))
          ("remote-branch" (nth 2 remote))
          ("partial-update" (if partial-update "yes" "no"))))
     script-file)))

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
           (replace-regexp-in-string
            "#\\+\\+" "#+"
            "# -*- coding: utf-8-unix; -*-
#++TITLE:       %s
#++AUTHOR:      %s
#++EMAIL:       %s
#++DATE:        %s

# #++URI:         %s
# #++KEYWORDS:    %s
# #++TAGS:        %s
# #++DESCRIPTION: %s

#++LANGUAGE:    %s
#++OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
")
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
           7   ;; Set default level to 7 instead of `org-export-headline-levels'
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
   (let* ((p (owp/select-project-name "Which project do you want post? "))
          (c (read-string (format "Category of \"%s\" project: " p)
                          (owp/get-config-option :default-category)))
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
    (save-buffer)))



;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'org-webpage)

;;; org-webpage.el ends here
;; #+END_SRC
