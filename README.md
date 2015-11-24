- [org-webpage README](#org-webpage-readme)
  - [Installation](#installation)
  - [Configuration](#configuration)
  - [Publication](#publication)
  - [Dependencies](#dependencies)
  - [Known issues](#known-issues)

# org-webpage README<a id="orgheadline6"></a>

org-webpage is a static site generator based on [org-mode](http://orgmode.org/),
which code derived from Kelvin H's [org-page](https://github.com/kelvinh/org-page).

The main differents of two projects are as follow:

1.  org-webpage's core **don't hard code git**, its process is like below:

        [ Org files ] --( Export )--> [ Html files ] -------
              \                                             \
               \--------(Generate)--> [ Upload bash script] ---> ( Git repos )--\
                                             \                                   \
                                              \------------------------------------( Upload )---> Remote

2.  org-webpage's default config is \`org-publish-project-alist' style alist,
    which can manage multi-site configs in an emacs session easily.
3.  org-website find theme-files from a **themes-list** in sequence and same theme-file
    first found will be used. User can set **fallback theme** with the help of this feature.
4.  org-website include a tiny emacs web server, which can be used to test publish.
5.  &#x2026;

## Installation<a id="orgheadline1"></a>

org-webpage is now available from the famous emacs package repo [melpa](http://melpa.milkbox.net/)
so the recommended way is to install it through emacs package
management system. For more info about installation, please see
**tips.org** in the "doc" folder.

## Configuration<a id="orgheadline2"></a>

org-webpage use variable \`owp/project-config-alist' to store all projects's configures, user
can add a project with the help of \`add-to-list' function, but the easiest way is
using \`owp/add-project-config' function.

The follow code is [my website](http://tumashu.github.com)'s [config](https://github.com/tumashu/tumashu.github.com/blob/source/eh-website.el),
you can adjust and paste it to your `.emacs` file:

    the following is only needed if you install org-page manually
    (add-to-list 'load-path "path/to/org-webpage")

    (require 'org-webpage)

    (owp/add-project-config
     '("tumashu.github.com"
       :repository-directory "~/project/emacs-packages/tumashu.github.com"
       :remote (git "https://github.com/tumashu/tumashu.github.com.git" "master")
       :site-domain "http://tumashu.github.com/"
       :site-main-title "Tumashu 的个人小站"
       :site-sub-title "(九天十地，太上忘情！！！)"
       :theme (worg)
       :source-browse-url ("Github" "https://github.com/tumashu/tumashu.github.com")
       :personal-avatar "/media/img/horse.jpg"
       :personal-duoshuo-shortname "tumashu-website"
       :web-server-port 7654))

[Chinese-pyim](https://github.com/tumashu/chinese-pyim) 's org-webpage [config](https://github.com/tumashu/chinese-pyim/blob/master/chinese-pyim-devtools.el) is a more complex example.

You can find more config options and theirs default values by commands:

    C-h v owp/project-config-alist
    C-h v owp/config-fallback

## Publication<a id="orgheadline3"></a>

    M-x owp/do-publication

## Dependencies<a id="orgheadline4"></a>

1.  [emacs](http://www.gnu.org/software/emacs/): this is an "of-course" dependency
2.  [org mode](http://orgmode.org/): v8.0 is required, please use `M-x org-version <RET>` to make sure you org mode version is not less than 8.0
3.  [bash](http://www.gnu.org/software/bash/): the GNU Project's shell
4.  [git](http://git-scm.com): a free and open source version control system
5.  [mustache.el](https://github.com/Wilfred/mustache.el): a mustache templating library for Emacs
6.  [htmlize.el](http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi): a library for syntax highlighting (usually this library is shipped with emacs)
7.  [dash.el](https://github.com/magnars/dash.el): a modern list library for Emacs
8.  [ht.el](https://github.com/Wilfred/ht.el): a modern hash-table library for Emacs
9.  [web-server](https://github.com/eschulte/emacs-web-server): a web server library for Emacs

## Known issues<a id="orgheadline5"></a>

1.  Currently the deletion change handler has not been implemented so
    if you deleted some org sources, you may have to manually delete
    corresponding generated html files.
2.  URI path change detection is not available. That is, if you make a
    post with the URI "/blog/2013/03/25/the-old-post-name" and then
    change this value in your org source, org-webpage would be unable to
    detect that this has happened. it will only publish a new html
    file for you so you need to delete the old html file related to
    the old URI manually.
