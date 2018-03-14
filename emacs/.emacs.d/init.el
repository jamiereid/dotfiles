(require 'package)

;; Repos to use for packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; List of packages to ensure is installed
(setq package-list '(org
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages
   (quote
    (diminish org-bullets counsel use-package ivy flycheck expand-region company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "blue4" :foreground "gray90"))))
 '(mode-line-buffer-id ((t (:foreground "#f0dfaf" :weight ultra-bold))))
 '(mode-line-inactive ((t (:background "#404045" :foreground "gray60"))))
 '(org-block ((t (:background "#333333"))))
 '(org-block-background ((t (:background "#333333"))))
 '(org-block-begin-line ((t (:foreground "#6f6f6f" :background "#1e2320"))) t)
 '(org-block-end-line ((t (:foreground "#6f6f6f" :background "#1e2320"))) t)
 '(show-paren-match ((t (:background "default" :foreground "#ecbcbc" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "default" :foreground "#9fafaf" :weight ultra-bold))))
 '(which-func ((t (:foreground "orange")))))
