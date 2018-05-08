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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powershell jedi s elpy web-mode use-package rainbow-mode pandoc-mode org-pdfview org-bullets markdown-mode json-mode flycheck expand-region exec-path-from-shell diminish counsel company color-theme-zenburn color-theme-monokai color-theme-molokai)))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "blue4" :foreground "gray90"))))
 '(mode-line-buffer-id ((t (:foreground "#f0dfaf" :weight ultra-bold))))
 '(mode-line-inactive ((t (:background "#404045" :foreground "gray60"))))
 '(show-paren-match ((t (:background "default" :foreground "#ecbcbc" :weight ultra-bold))))
 '(show-paren-mismatch ((t (:background "default" :foreground "#9fafaf" :weight ultra-bold))))
 '(which-func ((t (:foreground "orange")))))
