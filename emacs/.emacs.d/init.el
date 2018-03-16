(require 'package)

;; Repos to use for packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; List of packages to ensure is installed
(setq package-list '(org
                     diminish
                     use-package))

;; set up load-paths and autoloads for installed packages so we can configure them
(package-initialize)

;; fetch available packages
(or
    (file-exists-p package-user-dir)
    (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))

(require 'use-package)
(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org"
                    user-emacs-directory))
