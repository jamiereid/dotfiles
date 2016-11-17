;; -*- mode: elisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom trust themes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "b2f1d127439a017454308a94a095bde87c5d7bc24ec2288b406b7ca943e77926" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" default)))
 '(package-selected-packages
   (quote
    (flycheck smart-mode-line-powerline-theme smart-mode-line jedi auto-complete markdown-mode python-mode helm magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

(setq package-list '(magit
                     helm
                     python-mode
                     markdown-mode
                     auto-complete
                     jedi
                     smart-mode-line
                     smart-mode-line-powerline-theme
                     flycheck))

;; add some package repos
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
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
      `((".*" . ,"~/.emacs-tmp/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-tmp/" t)))

;;;;;;;;;;;;;;;
;; mode line ;;
;;;;;;;;;;;;;;;

;; set smart-mode-line to powerline
(setq sml/theme 'powerline)

;; load smart-mode-lines
(sml/setup)

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

;; add to load path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; turn off splashscreen
(setq inhibit-splash-screen t)

;; turn off tool bar and scroll bar if we're in GUI mode
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;; turn off menu bar
(menu-bar-mode -1)

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

;; ido-mode
(ido-mode t)

;; recent mode
(recentf-mode 1)

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

;; set theme path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; load theme
(load-theme 'badwolf t)

;; default font
(set-default-font "Droid Sans Mono-12")

;;;;;;;;;;;;;;;;;;;
;; Auto-complete ;;
;;;;;;;;;;;;;;;;;;;

(ac-config-default)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;

;; Enable org-mode
(require 'org)

;; set some variables
(setq org-dir "~/gdrive/org/")

;; set up some key bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; keep track of when a TODO is closed and record a note
(setq org-log-done 'time)
;(setq org-log-done 'note)

;; turn on fast state selection
(setq org-use-fast-todo-selection t)

;; Define our agenda files
(setq org-agenda-files (list (concat org-dir "work.org")
                             (concat org-dir "home.org")
                             (concat org-dir "inbox.org")))

;; Default capture file
(setq default-notes-file (concat org-dir "inbox.org"))

;; Capture templates
; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-capture-templates
      '(("w" "Work" entry (file (concat org-dir "work.org"))
         "* TODO %? :WORK:\n ADDED:%U")
	("h" "Home" entry (file (concat org-dir "home.org"))
         "* TODO %? \n ADDED:%U")
        ("m" "Media" checkitem (file+headline (concat org-dir "home.org") "Media")
         "[ ] %?")))

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;
;; Aspell ;;
;;;;;;;;;;;;

(setq-default ispell-program-name "aspell")

;;;;;;;;;;;
;; tramp ;;
;;;;;;;;;;;

;; faster than default scp
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

;; nice option to find recent files
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; redefine M-x to use Helm
(global-set-key (kbd "M-x") 'helm-M-x)

;; more useful kill ring cycling
(global-set-key (kbd "C-y") 'helm-show-kill-ring)

;; file navigation on steroids
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; key bindings for org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)
