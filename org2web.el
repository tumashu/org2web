;;; org2web.el --- static site generator based on org mode

;; * Header
;; Copyright (C)  2016 Feng Shu, Jorge Javier Araya Navarro
;;                2015 Feng Shu
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Jorge Javier Araya Navarro <elcorreo AT deshackra.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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

;; * org2web README                                                 :README:
;; org2web is the new name of org-webpage, the reason of renaming org-webpage to org2web
;; is: [[https://github.com/purcell/package-lint/issues/75]]

;; org2web is a static site generator based on [[http://orgmode.org/][org-mode]],
;; which code derived from Kelvin H's [[https://github.com/kelvinh/org-page][org-page]].

;; The main differents of two projects are as follow:

;; 1. org2web's core *don't hard code git*, its process is like below:
;;    #+BEGIN_EXAMPLE

;;   [ Org files in repository]  [ Website project configure ]

;;                |                           |
;;           < Export >                 < Generate >
;;                |                           |

;;          [ HTML files ]               [ Uploader ]  <- ( Uploader is a bash script )

;;                |                           |
;;                |                           |
;;                +-------------+-------------+
;;                              |
;;                              |
;;                      < Run Uploader >  <- ( For example: git uploader, rclone uploader or others )
;;                              |
;;                              |

;;                          [ REMOTE ]

;;    #+END_EXAMPLE

;; 2. org2web's default config is `org-publish-project-alist' style alist,
;;    which can manage multi-site configs in an emacs session easily.
;; 3. org-website find theme-files from a *themes-list* in sequence and same theme-file
;;    first found will be used. User can set *fallback theme* with the help of this feature.
;; 4. org-website include a tiny emacs web server, which can be used to test publish.
;; 5. org-website can use other uploaders to upload website, for example: rclone.
;; 6. ...

;; ** Installation

;; org2web is now available from the famous emacs package repo [[http://melpa.milkbox.net/][melpa]]
;; so the recommended way is to install it through emacs package
;; management system. For more info about installation, please see
;; *tips.org* in the "doc" folder.

;; ** Configuration
;; org2web use variable `org2web-projects' to store all projects's configures, user
;; can add a project with the help of `add-to-list' function, but the easiest way is
;; using `org2web-add-project' function.

;; The follow code is [[http://tumashu.github.com][my website]]'s [[https://github.com/tumashu/tumashu.github.com/blob/source/eh-website.el][config]],
;; you can adjust and paste it to your =.emacs= file:

;; #+BEGIN_EXAMPLE
;; (add-to-list 'load-path "path/to/org2web") ; Only needed if you install org2web manually

;; (require 'org2web)

;; (org2web-add-project
;;  '("tumashu.github.com"
;;    :repository-directory "~/project/emacs-packages/tumashu.github.com"
;;    :remote (git "https://github.com/tumashu/tumashu.github.com.git" "master")
;;    ;; you can use `rclone` with `:remote (rclone "remote-name" "/remote/path/location")` instead.
;;    :site-domain "http://tumashu.github.com/"
;;    :site-main-title "Tumashu 的个人小站"
;;    :site-sub-title "(九天十地，太上忘情！！！)"
;;    :theme (worg)
;;    :source-browse-url ("Github" "https://github.com/tumashu/tumashu.github.com")
;;    :personal-avatar "/media/img/horse.jpg"
;;    :personal-duoshuo-shortname "tumashu-website"
;;    :web-server-port 7654))
;; #+END_EXAMPLE

;; [[https://github.com/tumashu/pyim][pyim]] 's org2web [[https://github.com/tumashu/pyim/blob/master/pyim-devtools.el][config]] is a more complex example.

;; You can find more config options and theirs default values by commands:

;; #+BEGIN_EXAMPLE
;; C-h v org2web-projects
;; C-h v org2web-config-fallback
;; #+END_EXAMPLE

;; ** Publication

;; #+BEGIN_EXAMPLE
;; M-x org2web-publish
;; #+END_EXAMPLE

;; ** Dependencies

;; 1. [[http://www.gnu.org/software/emacs/][emacs]]: this is an "of-course" dependency
;; 2. [[http://orgmode.org/][org mode]]: v8.0 is required, please use =M-x org-version <RET>= to make sure you org mode version is not less than 8.0
;; 3. [[http://www.gnu.org/software/bash/][bash]]: the GNU Project's shell
;; 4. [[http://git-scm.com][git]]: a free and open source version control system
;; 5. [[http://rclone.org/downloads/][rclone]]: support to other remote locations, see rclone's overview for more information. (Optional)
;; 6. [[https://github.com/Wilfred/mustache.el][mustache.el]]: a mustache templating library for Emacs
;; 7. [[http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi][htmlize.el]]: a library for syntax highlighting (usually this library is shipped with emacs)
;; 8. [[https://github.com/magnars/dash.el][dash.el]]: a modern list library for Emacs
;; 9. [[https://github.com/Wilfred/ht.el][ht.el]]: a modern hash-table library for Emacs
;; 10. [[https://github.com/skeeto/emacs-web-server][simple-httpd]]: Extensible Emacs HTTP 1.1 server

;; ** Known issues

;; 1. Currently the deletion change handler has not been implemented so
;;    if you deleted some org sources, you may have to manually delete
;;    corresponding generated html files.
;; 2. URI path change detection is not available. That is, if you make a
;;    post with the URI "/blog/2013/03/25/the-old-post-name" and then
;;    change this value in your org source, org2web would be unable to
;;    detect that this has happened. it will only publish a new html
;;    file for you so you need to delete the old html file related to
;;    the old URI manually.


;;; Code:

;; * 代码说明                                                             :code:

(require 'cl-lib)
(require 'ox)
(require 'ht)
(require 'org2web-util)
(require 'org2web-vars)
(require 'org2web-config)
(require 'org2web-resource)
(require 'org2web-export)
(require 'org2web-webserver)
(require 'org2web-el2org)


(defconst org2web-version "0.1")

;;;###autoload
(defun org2web-add-project (project-config)
  "Add `project-config' to `org2web-projects'"
  (if (listp project-config)
      (let ((project-name (car project-config)))
        (when (stringp project-name)
          (setq org2web-projects
                (remove (assoc project-name org2web-projects)
                        org2web-projects)))
        (add-to-list 'org2web-projects project-config))
    (message "Invalid project config!")))

;;;###autoload
(defun org2web-select-project (prompt &optional project-name)
  "Let user select a project then return its name."
  (setq org2web-current-project nil)
  (setq project-name
        (or project-name
            org2web-default-project
            (completing-read prompt
                             (delete-dups
                              (mapcar 'car org2web-projects))
                             nil t nil nil org2web-last-project)))
  (setq org2web-current-project project-name
        org2web-last-project project-name)
  project-name)

;;;###autoload
(defun org2web-publish (&optional project-name publishing-directory job-number update-top-n)
  (interactive)
  (setq project-name (org2web-select-project "Which project do you want to publish? " project-name))
  (setq org2web-item-cache nil)

  (org2web-verify-configuration)
  (let* ((remote (org2web-get-config-option :remote))
         (uploader-config (cdr (assoc (nth 0 remote) org2web-uploaders)))
         (support-partial-update (plist-get uploader-config :support-partial-update))
         (jobs (if support-partial-update
                   '((1 . "Full publish")
                     (2 . "Partial publish")
                     (3 . "Test full publish")
                     (4 . "Test partial publish")
                     (5 . "Upload latest publish"))
                 '((1 . "Full publish")
                   (2 . "Test full publish")
                   (3 . "Upload latest publish"))))
         (job-used (completing-read
                    "Which job do you want to active: "
                    (mapcar #'cdr jobs)))
         (job-number (car (rassoc job-used jobs)))
         (test-publish (if support-partial-update
                           (or (= job-number 3)
                               (= job-number 4))
                         (= job-number 2)))
         (partial-update (and support-partial-update
                              (or (= job-number 2)
                                  (= job-number 4))))
         (upload-latest-publish (if support-partial-update
                                    (= job-number 5)
                                  (= job-number 3)))
         (repo-dir (org2web-get-repository-directory))
         (publish-root-dir (org2web-get-uploader-directory project-name))
         (export-dir (org2web-get-uploader-directory project-name "export"))
         (history-dir (org2web-get-uploader-directory project-name "history"))
         (publishing-directory
          (when publishing-directory
            (expand-file-name publishing-directory)))
         (publish-dir
          (or publishing-directory
              (org2web-get-uploader-directory project-name "publish")
              (org2web-get-publishing-directory)))
         (test-publish-dir (org2web-get-uploader-directory project-name "test-publish"))
         (uploader-file (concat publish-root-dir "org2web-uploader.sh"))
         (site-domain (org2web-get-site-domain))
         (preparation-function (org2web-get-config-option :preparation-function))
         (repo-files
          (unless upload-latest-publish
            (when preparation-function
              (run-hooks 'preparation-function))
            (org2web-sort-files (org2web-remove-matched-items
                                 (org2web-directory-files-recursively repo-dir "\\.org$")
                                 (org2web-get-config-option :ignore)))))
         (length-repo-files (length repo-files))
         (update-top-n
          (cond ((and partial-update (numberp update-top-n)) update-top-n)
                (partial-update (org2web-read-top-n
                                 "org2web will update TOP (N) org-files, Please type N: "
                                 repo-files repo-dir))))
         (changed-files (if (numberp update-top-n)
                            (cl-subseq repo-files 0 (min update-top-n length-repo-files))
                          repo-files)))

    (if upload-latest-publish
        (org2web-delete-directory
         history-dir publish-dir test-publish-dir)
      (org2web-delete-directory publish-root-dir)
      (org2web-make-directory export-dir))

    (org2web-make-directory
     history-dir publish-dir test-publish-dir)

    (if test-publish
        (let ((org2web-always-use-relative-url t) ; Local test website, can't use absolute path.
              (port (or (org2web-get-config-option :web-server-port)
                        (org2web-get-random-number 4))))
          (org2web-prepare-theme-resources test-publish-dir)
          (org2web-publish-changes repo-files changed-files test-publish-dir)
          (org2web-webserver-browse test-publish-dir port))
      (unless upload-latest-publish
        (org2web-prepare-theme-resources export-dir)
        (org2web-publish-changes repo-files changed-files export-dir))
      (org2web-generate-and-run-uploader
       uploader-file remote export-dir history-dir publish-dir partial-update))))

(defun org2web-generate-and-run-uploader (uploader-file remote export-dir history-dir publish-dir partial-update)
  "Generate shell script UPLOADER-FILE then RUN it, the uploader is used to upload html files
generated by org2web to REMOTE."
  (if (not (and uploader-file remote export-dir history-dir publish-dir))
      (message "Can't generate org2web uploader file.")
    (let* ((uploader-name (nth 0 remote))
           (uploader-names (mapcar #'car org2web-uploaders))
           (uploader-config (cdr (assoc uploader-name org2web-uploaders)))
           (uploader-requires (plist-get uploader-config :requires))
           (uploader-help-info (plist-get uploader-config :help-info))
           (uploader-template
            (org2web-file-to-string
             (org2web-get-uploader-template
              (or (plist-get uploader-config :template)
                  (concat (symbol-name uploader-name) ".mustache")))))
           (uploader-template-settings
            (plist-get uploader-config :template-settings)))
      (if (not (member uploader-name uploader-names))
          (message (format "Uploader name %S in :remote is not recognized, please choice one from %S."
                           uploader-name uploader-names))
        (if (cl-some #'(lambda (x)
                         (not (executable-find x)))
                     uploader-requires)
            (message uploader-help-info)
          ;; Generate uploader file
          (org2web-string-to-file
           (mustache-render
            uploader-template
            (funcall uploader-template-settings ; it's a function.
                     remote export-dir history-dir publish-dir partial-update))
           uploader-file)
          ;; Run Uploader file
          (if (and (file-exists-p uploader-file)
                   (executable-find "bash"))
              (cond
               ((string-equal system-type "windows-nt")
                (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" uploader-file t t)))
               ((and (string-equal system-type "gnu/linux")
                     org2web-terminal-emulater)
                (start-process-shell-command
                 "org2web-uploader-script"
                 nil
                 (format "%s -e 'bash %s'"
                         org2web-terminal-emulater
                         uploader-file))))
            (message "Can't run org2web uploader, user should install 'bash' correctly.")))))))

(defun org2web-verify-configuration ()
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
  (unless (member org2web-current-project
                  (mapcar 'car org2web-projects))
    (error "Can't find project: \"%s\"" org2web-current-project))
  (let ((repo-dir (org2web-get-repository-directory))
        (site-domain (org2web-get-site-domain)))
    (unless (and repo-dir (file-directory-p repo-dir))
      (error "Repository directory is not properly configured."))
    (unless site-domain
      (error "Site domain is not properly configured."))))

(defun org2web-generate-readme (save-dir)
  "Generate README for `org2web-new-repository'. SAVE-DIR is the directory where to
save generated README."
  (org2web-string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and org2web."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by org2web \"org2web-new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by org2web.")
   (expand-file-name "README" save-dir)))

(defun org2web-generate-index (save-dir)
  "Generate index.org for `org2web-new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (org2web-string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun org2web-generate-about (save-dir)
  "Generate about.org for `org2web-new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (org2web-string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by org2web.")
   (expand-file-name "about.org" save-dir)))

(defun org2web-insert-options-template (&optional title uri
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
                                 `((?c . ,(org2web-get-config-option :default-category))
                                   (?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(org2web-encode-string-to-url i)))))))
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

;;;###autoload
(defun org2web-new-post (&optional project-name category filename insert-fallback-template)
  "Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive
   (let* ((p (org2web-select-project "Which project do you want post? "))
          (c (read-string (format "Category of \"%s\" project: " p)
                          (org2web-get-config-option :default-category)))
          (f (read-string (format "Filename of \"%s\" project: " p) "new-post.org" p))
          (d (yes-or-no-p "Insert fallback template? ")))
     (list p c f d)))
  (if (string= category "")
      (setq category (org2web-get-config-option :default-category)))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (org2web-string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((repo-dir (org2web-get-repository-directory))
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
        (call-interactively 'org2web-insert-options-template)
      (org2web-insert-options-template "<Insert Your Title Here>"
                                       (format "/%s/%%y/%%m/%%d/%%t/ Or /%s/%%t/"
                                               category category)
                                       "keyword1, keyword2, keyword3"
                                       "tag1, tag2, tag3"
                                       "<Add description here>"))
    (save-buffer)))




;; * Footer
(provide 'org2web)

;;; org2web.el ends here
