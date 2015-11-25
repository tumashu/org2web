;;; owp-lentic.el --- lentic support for org-webpage

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu  <tumashu AT 163.com>
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

;; * 说明文档                                                           :README:

;; 这个文件包含一些辅助工具，这些工具可以让 org-webpage 与 [[https://github.com/phillord/lentic][lentic]] 更方便的集成。

;; ** 安装

;; *** 安装 lentic
;; lentic 是一个比较有意思的工具，下面是它的作者对它的介绍：

;; #+BEGIN_EXAMPLE
;; Lentic allows two buffers to share the same or similar content
;; but otherwise operate independently. This can be used for several
;; different purposes. Different buffers can be in the same mode,
;; with different locations of point, even different text sizes -- in effect,
;; providing multiple persistent views
;; #+END_EXAMPLE

;; lentic 功能很多，在这里，我仅仅把它当做一个用 org-mode 格式写 comment 的工具。

;; 安装方式：

;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET lentic RET

;; *** 安装 ox-gfm

;; ox-gfm (Github Flavored Markdown exporter for Org Mode) 可以将 org 文档转换为 github markdown 格式。
;; 目前，ox-gfm 已经合并到 org-plus-contrib 包中了，而 melpa 中可以搜索到的 ox-gfm 包已经 *过期* 了。

;; 安装方式：

;; 1. 配置 org 源，具体请参考：http://orgmode.org/elpa.html
;; 2. M-x package-install RET org-plus-contrib RET

;; ** 配置 owp-lentic
;; #+BEGIN_EXAMPLE
;; (require 'owp-lentic) ;; You need install lentic and gfm
;; (owp/lentic-mode-setup) ;; Enable `owp/lentic-mode' for `emacs-lisp-mode' and `org-mode'
;; #+END_EXAMPLE

;; ** 使用

;; *** 用 `owp/lentic-switch-window' 命令写 org 格式的 Comment
;; 编辑 emacs-lisp 文件的时时，按 'C-c j' 快捷键会弹出一个 org-mode 窗口，这个窗口
;; 显示的内容和 emacs-lisp 文件内容在逻辑上具有高度的相似性。

;; 文件不同的部份，在不同的地方编辑， comment 在 org buffer 中编辑，而 elisp 到 emacs-lisp buffer
;; 中编辑，两个 buffer 中的内容实时自动的同步。

;; *** 让 org-webpage 处理 lentic 化的 emacs-lisp 文件

;; TODO

;;; Code:

;; * 代码                                                                 :code:

;; ** Require

;; #+BEGIN_SRC emacs-lisp
(require 'easy-lentic)
(require 'ox-gfm)
;; #+END_SRC

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-generate-readme (&optional project-name)
  (interactive)
  (owp/select-project-name
   "Which project do you want to generate README.md? " project-name)
  (easy-lentic-generate-file
   (owp/get-repository-directory)
   (car (owp/get-config-option :lentic-readme-sources))
   (owp/get-config-option :lentic-readme-tags)
   'gfm "README.md"))

(defun owp/lentic-generate-index (&optional project-name)
  (interactive)
  (owp/select-project-name
   "Which project do you want to generate index.org? " project-name)
  (easy-lentic-generate-file
   (owp/get-repository-directory)
   (car (owp/get-config-option :lentic-index-sources))
   (owp/get-config-option :lentic-index-tags)
   'org "index.org"))
;; #+END_SRC

;; ** org-webpage 导出函数（支持 lentic）

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-org-export-function ()
  "A function with can export org file to html."
  (let ((org-export-before-processing-hook
         '(easy-lentic-org-export-preprocess))
        (org-export-filter-paragraph-functions
         '(easy-lentic-org-clean-space))
        (org-export-select-tags (owp/get-config-option :lentic-doc-tags))
        (org-export-headline-levels 7)
        (indent-tabs-mode nil)
        (tab-width 4))
    (org-export-as 'html nil nil t nil)))

(defun owp/lentic-preparation-function ()
  "Generate org files by lentic."
  ;; Orgify elisp files
  (let* ((repo-dir (owp/get-repository-directory))
         (regexp-list (owp/get-config-option :lentic-doc-sources))
         (files (when regexp-list
                  (owp/select-matched-items
                   (owp/directory-files-recursively repo-dir "\\.el$")
                   regexp-list))))
    (when files
      (mapc #'easy-lentic-orgify-if-necessary files)))

  ;; Generate README.mk if necessary
  (let ((repo-dir (owp/get-repository-directory))
        (file (car (owp/get-config-option :lentic-readme-sources)))
        (tags (owp/get-config-option :lentic-readme-tags)))
    (when file
      (easy-lentic-generate-file
       repo-dir file tags 'gfm "README.md")))

  ;; Generate index.org if necessary
  (let ((repo-dir (owp/get-repository-directory))
        (file (car (owp/get-config-option :lentic-index-sources)))
        (tags (owp/get-config-option :lentic-index-tags)))
    (when file
      (easy-lentic-generate-file
       repo-dir file tags 'org "index.org"))))
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'owp-lentic)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; owp-lentic.el ends here
;; #+END_SRC
