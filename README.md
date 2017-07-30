- [OWP README](#org85442a7)
  - [Installation](#orgd9e21d2)
  - [Configuration](#org8d0d6fd)
  - [Publication](#org2c76f04)
  - [Dependencies](#org039c031)
  - [Known issues](#orgb78e717)


<a id="org85442a7"></a>

# OWP README

OWP is the new name of org-webpage, the reason of renaming org-webpage to OWP is: <https://github.com/purcell/package-lint/issues/75>

OWP is a static site generator based on [org-mode](http://orgmode.org/), which code derived from Kelvin H's [org-page](https://github.com/kelvinh/org-page).

The main differents of two projects are as follow:

1.  OWP's core **don't hard code git**, its process is like below:


        [ Org files in repository]  [ Website project configure ]

                     |                           |
                < Export >                 < Generate >
                     |                           |

               [ HTML files ]               [ Uploader ]  <- ( Uploader is a bash script )

                     |                           |
                     |                           |
                     +-------------+-------------+
                                   |
                                   |
                           < Run Uploader >  <- ( For example: git uploader, rclone uploader or others )
                                   |
                                   |

                               [ REMOTE ]

2.  OWP's default config is \`org-publish-project-alist' style alist, which can manage multi-site configs in an emacs session easily.
3.  org-website find theme-files from a **themes-list** in sequence and same theme-file first found will be used. User can set **fallback theme** with the help of this feature.
4.  org-website include a tiny emacs web server, which can be used to test publish.
5.  org-website can use other uploaders to upload website, for example: rclone.
6.  &#x2026;


<a id="orgd9e21d2"></a>

## Installation

OWP is now available from the famous emacs package repo [melpa](http://melpa.milkbox.net/) so the recommended way is to install it through emacs package management system. For more info about installation, please see **tips.org** in the "doc" folder.


<a id="org8d0d6fd"></a>

## Configuration

OWP use variable \`owp-projects' to store all projects's configures, user can add a project with the help of \`add-to-list' function, but the easiest way is using \`owp-add-project' function.

The follow code is [my website](http://tumashu.github.com)'s [config](https://github.com/tumashu/tumashu.github.com/blob/source/eh-website.el), you can adjust and paste it to your `.emacs` file:

    (add-to-list 'load-path "path/to/owp") ; Only needed if you install owp manually

    (require 'owp)

    (owp-add-project
     '("tumashu.github.com"
       :repository-directory "~/project/emacs-packages/tumashu.github.com"
       :remote (git "https://github.com/tumashu/tumashu.github.com.git" "master")
       ;; you can use `rclone` with `:remote (rclone "remote-name" "/remote/path/location")` instead.
       :site-domain "http://tumashu.github.com/"
       :site-main-title "Tumashu 的个人小站"
       :site-sub-title "(九天十地，太上忘情！！！)"
       :theme (worg)
       :source-browse-url ("Github" "https://github.com/tumashu/tumashu.github.com")
       :personal-avatar "/media/img/horse.jpg"
       :personal-duoshuo-shortname "tumashu-website"
       :web-server-port 7654))

[pyim](https://github.com/tumashu/pyim) 's owp [config](https://github.com/tumashu/pyim/blob/master/pyim-devtools.el) is a more complex example.

You can find more config options and theirs default values by commands:

    C-h v owp-projects
    C-h v owp-config-fallback


<a id="org2c76f04"></a>

## Publication

    M-x owp-do-publication


<a id="org039c031"></a>

## Dependencies

1.  [emacs](http://www.gnu.org/software/emacs/): this is an "of-course" dependency
2.  [org mode](http://orgmode.org/): v8.0 is required, please use `M-x org-version <RET>` to make sure you org mode version is not less than 8.0
3.  [bash](http://www.gnu.org/software/bash/): the GNU Project's shell
4.  [git](http://git-scm.com): a free and open source version control system
5.  [rclone](http://rclone.org/downloads/): support to other remote locations, see rclone's overview for more information. (Optional)
6.  [mustache.el](https://github.com/Wilfred/mustache.el): a mustache templating library for Emacs
7.  [htmlize.el](http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi): a library for syntax highlighting (usually this library is shipped with emacs)
8.  [dash.el](https://github.com/magnars/dash.el): a modern list library for Emacs
9.  [ht.el](https://github.com/Wilfred/ht.el): a modern hash-table library for Emacs
10. [simple-httpd](https://github.com/skeeto/emacs-web-server): Extensible Emacs HTTP 1.1 server


<a id="orgb78e717"></a>

## Known issues

1.  Currently the deletion change handler has not been implemented so if you deleted some org sources, you may have to manually delete corresponding generated html files.
2.  URI path change detection is not available. That is, if you make a post with the URI "/blog/2013/03/25/the-old-post-name" and then change this value in your org source, owp would be unable to detect that this has happened. it will only publish a new html file for you so you need to delete the old html file related to the old URI manually.


Converted from owp.el by [el2org](https://github.com/tumashu/el2org) .