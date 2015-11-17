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

;; * 说明文档

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

;; 安装方式：

;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET ox-gfm RET

;; ** 配置 owp-lentic
;; #+BEGIN_EXAMPLE
;; (require 'owp-lentic) ;; you need install lentic and gfm
;; (owp/lentic-define-switch-window-key "\C-cj") ;; set keybinding for `owp/lentic-switch-window'
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
(require 'lentic)
(require 'lentic-org)
(require 'lentic-doc)
(require 'ox-gfm)
(require 'ox-org)
;; #+END_SRC

;; ** 定义一个 org-webpage 专用的 lentic el2org 转换器
;; lentic 内置了两个 el2org 转换器函数：

;; 1. `lentic-el-org-init'
;; 2. `lentic-el-orgel-init'

;; 第一个转换器太过简单，不能处理 “^;;; ”, 而第二个转换器又太复杂，稳定性不好，所以
;; 在这里，我基于 `lentic-el-org-init' 自定义了一个转换器：`owp/lentic-el2org-init',
;; 这个转换器和 `lentic-el-org-init' 转换器的功能类似，仅仅对 “^;;; ” 特殊处理。

;; #+BEGIN_SRC emacs-lisp
(defclass owp/lentic-org2el-configuration
  (lentic-unmatched-chunk-configuration
   lentic-uncommented-chunk-configuration)
  ())

(defmethod lentic-clone
  ((conf owp/lentic-org2el-configuration)
   &optional start stop length-before
   start-converted stop-converted)
  (let ((clone-return (call-next-method conf)))
    ;; replace all ';;; '  to ';;; '.
    (m-buffer-replace-match
     (m-buffer-match
      (lentic-that conf)
      "^ *;; ;;; ")
     ";;; ")
    clone-return))

(defmethod lentic-invert
  ((conf owp/lentic-org2el-configuration))
  (lentic-m-oset
   (lentic-el2org-init)
   :that-buffer
   (lentic-this conf)))

(defun owp/lentic-org2el-init ()
  (lentic-org-oset
   (owp/lentic-org2el-configuration
    "owp-lb-org2el"
    :lentic-file
    (concat
     (file-name-sans-extension
      (buffer-file-name))
     ".el"))))

(add-to-list 'lentic-init-functions
             'owp/lentic-org2el-init)

(defclass owp/lentic-el2org-configuration
  (lentic-unmatched-chunk-configuration
   lentic-commented-chunk-configuration)
  ())

(defmethod lentic-create
  ((conf owp/lentic-el2org-configuration))
  (let ((buf (call-next-method conf)))
    (with-current-buffer buf
      (show-all)
      ;; After run `show-all', the point move to
      ;; the end of buffer, reupdate the point.
      (lentic-update-point conf))
    buf))

(defmethod lentic-invert
  ((conf owp/lentic-el2org-configuration))
  (lentic-m-oset
   (owp/lentic-org2el-init)
   :delete-on-exit t
   :that-buffer (lentic-this conf)))

(defun owp/lentic-el2org-init ()
  (lentic-org-oset
   (owp/lentic-el2org-configuration
    "owp-lb-el2org"
    ;; we don't really need a file and could cope without, but org mode assumes
    ;; that the buffer is file name bound when it exports. As it happens, this
    ;; also means that file saving is possible which in turn saves the el file
    :lentic-file
    (concat
     (file-name-sans-extension
      (buffer-file-name))
     ".org"))))

(add-to-list 'lentic-init-functions
             'owp/lentic-el2org-init)

(defun owp/lentic-switch-window ()
  (interactive)
  (when (derived-mode-p 'emacs-lisp-mode)
    ;; Set buffer-local variable `lentic-init'
    (setq lentic-init '(owp/lentic-el2org-init)))
  (when (derived-mode-p 'org-mode)
    ;; Set buffer-local variable `lentic-init'
    (setq lentic-init '(owp/lentic-org2el-init)))
  (lentic-mode-create-from-init)
  (lentic-mode-split-window-below)
  (let ((window
         (get-buffer-window
          (lentic-mode-find-next-visible-lentic-buffer
           (current-buffer)))))
    (when (window-live-p window)
      (select-window window))))

(defun owp/lentic-define-switch-window-key (key)
  (require 'org)
  (require 'lisp-mode)
  (define-key org-mode-map (read-kbd-macro key)
    'owp/lentic-switch-window)
  (define-key emacs-lisp-mode-map (read-kbd-macro key)
    'owp/lentic-switch-window))
;; #+END_SRC


;; ** 定义一个 org export 过滤器，处理中文文档中的多余空格

;; org 文档导出为 HTML 文档时，中文与中文之间的回车符默认会转化为空格符，
;; 这些空格对于中文而言是多余的。所以我们定义了一个过滤器，当 org 文档导
;; 出为 HTML 或者 markdown 文档时，自动清除中文与中文之间不必要的空格。

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-org-clean-space (text backend info)
  "在export为HTML时，删除中文之间不必要的空格"
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) +\\(<\\)" regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(>\\) +\\(%s\\)" regexp)
             "\\1\\2" string))
      string)))
;; #+END_SRC

;; ** 清洗 lentic 转换得到的 org 文件

;; 在编辑 emacs-lisp 文件时，有许多 *约定俗成* 的东西，而许多相关工具，
;; 又依赖这些 *约定俗成* , 比较常见的有：

;; 1. 文档说明的开始，添加；“;;; Commentary:” 注释
;; 2. 代码部份的开始，添加：“;;; Code:” 注释

;; 这些书写习惯在 lentic 转换阶段处理起来非常麻烦，所以我将其原样输出
;; 到 org 文件，然后在 org 导出的时候用 `org-export-before-processing-hook'
;; 做处理，这样处理起来相对简单，下面是这个 hook 的定义：

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-org-export-preprocess (backend)
  "This function delete useless strings in org files which are converted from
emacs-lisp files by lentic."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^;;; +Commentary:.*" nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (re-search-forward "^;;; +Code:.*" nil t)
      (replace-match "" nil t))))

;; #+END_SRC

;; ** 根据 elisp 文件的 Commentary，生成 README 文件

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-orgify-if-necessary (el-file)
  (let* ((el-buffer (get-file-buffer el-file))
         (org-file (concat (file-name-sans-extension el-file) ".org"))
         (org-buffer (get-file-buffer org-file))
         (locked (or (file-locked-p el-file)
                     (file-locked-p org-file))))
    (when el-buffer
      (kill-buffer el-buffer))
    (when org-buffer
      (kill-buffer org-buffer))
    (unless locked
      (when (file-newer-than-file-p el-file org-file)
        (let ((lentic-kill-retain t))
          (lentic-batch-clone-and-save-with-config
           el-file 'owp/lentic-el2org-init))))))

(defun owp/lentic-generate-file (el-filename backend filename)
  (let ((repo-dir (owp/get-repository-directory))
        el-file org-file)
    (when (and repo-dir el-filename backend filename)
      (setq el-file (concat repo-dir el-filename))
      (setq org-file (concat (file-name-sans-extension el-file) ".org"))
      (owp/lentic-orgify-if-necessary el-file)
      (if (file-exists-p org-file)
          (with-current-buffer (find-file-noselect org-file)
            (let ((org-export-filter-paragraph-functions '(owp/lentic-org-clean-space))
                  (org-export-before-processing-hook '(owp/lentic-org-export-preprocess))
                  (org-export-select-tags '("README"))
                  (org-export-with-tags nil)
                  (indent-tabs-mode nil)
                  (tab-width 4))
              (org-export-to-file backend filename)))
        (message "Generate %s fail!!!" filename)))))

(defun owp/lentic-generate-readme (&optional project-name)
  (interactive)
  (setq project-name
        (or project-name
            owp/default-project-name
            (completing-read "Which project do you want to publish? "
                             (delete-dups
                              (mapcar 'car owp/project-config-alist))
                             nil t nil nil owp/last-project-name)))
  (setq owp/current-project-name project-name
        owp/last-project-name project-name)
  (owp/lentic-generate-file
   (owp/get-config-option :lentic-readme-source) 'gfm "README.md")
  (setq owp/current-project-name nil))

;; #+END_SRC

;; ** org-webpage 导出函数（支持 lentic）

;; #+BEGIN_SRC emacs-lisp
(defun owp/lentic-org-export-function ()
  "A function with can export org file to html."
  (let ((org-export-before-processing-hook
         '(owp/lentic-org-export-preprocess))
        (org-export-filter-paragraph-functions
         '(owp/lentic-org-clean-space))
        (org-export-select-tags '("README" "devel" "doc" "code"))
        (org-export-headline-levels 7)
        (indent-tabs-mode nil)
        (tab-width 4))
    (org-export-as 'html nil nil t nil)))

(defun owp/lentic-preparation-function ()
  "Generate org files by lentic."
  (let* ((repo-dir (owp/get-repository-directory))
         (el-files (directory-files
                    repo-dir t (owp/get-config-option :lentic-doc-source)))
         (el-file-for-readme (owp/get-config-option :lentic-readme-source))
         (el-file-for-index (owp/get-config-option :lentic-index-source)))

    ;; Orgify elisp files
    (mapc #'owp/lentic-orgify-if-necessary el-files)

    ;; Generate README.mk if necessary
    (when el-file-for-readme
      (owp/lentic-generate-file el-file-for-readme 'gfm "README.mk"))

    ;; Generate index.org if necessary
    (when el-file-for-index
      (owp/lentic-generate-file el-file-for-index 'org "index.org"))))
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'owp-lentic)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; owp-lentic.el ends here
;; #+END_SRC
