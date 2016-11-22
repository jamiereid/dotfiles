;; -*- mode: elisp -*-

;;;;;;;;;;;;;;;;;;;;;
;; safe theme load ;;
;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;; Package init and installation
(require 'package)

;; Repos to use for packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; List of packages to ensure is installed
(setq package-list '(auto-complete
                     counsel
                     flycheck
                     go-mode
                     ivy
                     swiper))

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

;;;;;;;;;;;;;;;
;; Droppings ;;
;;;;;;;;;;;;;;;

(setq backup-directory-alist
    `((",*" . ,"~/.emacs-tmp/")))
(setq auto-save-file-name-transforms
    `((".*" ,"~/.emacs-tmp/" t)))

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

;; add to the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; declutter
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)))

;; show which function the pointer is in
(which-function-mode 1)

;; highlight pairs of parenthesis
(show-paren-mode 1)
(set-face-background 'show-paren-match-face "#aaaaaa")
(set-face-attribute 'show-paren-match-face nil 
        :weight 'bold :underline nil :overline nil :slant 'normal)

;; automagically add closing paren/bracket/brace/etc
(electric-pair-mode 1)

;; reload files if changed on disk but not in buffer
(global-auto-revert-mode 1)

;; show column numbers in status bar
(column-number-mode 1)

;; always indent using spaces
(setq-default indent-tabs-mode nil)

;; turn linum-mode on globally
(global-linum-mode t)

;; but also turn if off for some modes (scratch, orgmode)
(require 'linum-off)

;;make linum more readable
(setq linum-format " %3i ")

;; visual-line-mode
(global-visual-line-mode 1)

;;;;;;;;;;;;;;;;;;;
;; Auto-complete ;;
;;;;;;;;;;;;;;;;;;;

(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy, counsel, and swiper ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; other-window is M-o
(global-set-key "\M-o" 'other-window)

;; redefine C-x C-b to buffer-menu
(global-set-key "\C-x\C-b" 'buffer-menu)

;; indent regions more easily
(global-set-key "\C-ci" 'indent-region)

;; resize windows
(global-set-key (kbd "C-x <up>") 'shrink-window)
(global-set-key (kbd "C-x <down>") 'enlarge-window)
(global-set-key (kbd "C-x <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") 'enlarge-window-horizontally)

;;;;;;;;;;;;
;; syntax ;;
;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;;;;;;;;;;;;
;; golang ;;
;;;;;;;;;;;;
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

;; set theme path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; load theme
(load-theme 'cyberpunk t)

;; default font
(set-default-font "Droid Sans Mono-12")

;;;;;;;;;;;;;;;
;; mode line ;;
;;;;;;;;;;;;;;;
