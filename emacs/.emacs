;; -*- mode: elisp -*-

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

(setq package-list '(magit
                     helm
                     python-mode
                     markdown-mode
                     auto-complete
                     jedi))

;; add some package repos
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
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

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

;; add to load path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; turn off splashscreen
(setq inhibit-splash-screen t)

;; turn off tool bar
(tool-bar-mode -1)

;; turn off menu bar
(menu-bar-mode -1)

;; Ditch scrollbars
(scroll-bar-mode -1)

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
(load-theme 'zenburn t)

;; default font
(set-default-font "Droid Sans Mono-8")

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

;; ;; Enable org-mode
;; (require 'org)
;; ;; Make org-mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; ;; The above is the default in recent emacsen

;; ;; Set some org-mode stuff
;; (setq org-agenda-files (list "~/Dropbox/org/work.org"
;; 			     "~/Dropbox/org/home.org"
;; 			     "~/Dropbox/org/projects.org"
;; 			     "~/Dropbox/org/bts.org"
;; 			     "~/Dropbox/org/notes.org"
;; 			     "~/Dropbox/org/refile.org"
;; 			     "~/Dropbox/org/mhv.org"))

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; (define-key global-map "\C-cc" 'org-capture)

;; ;; Org Capture
;; (setq org-default-notes-file "~/Dropbox/org/refile.org")
;; (setq org-capture-templates
;;       '(("w" "Work" entry (file "~/Dropbox/org/refile.org")
;;          "* TODO %? :WORK:\n ADDED:%U")
;; 	("h" "Home" entry (file "~/Dropbox/org/refile.org")
;;          "* TODO %? \n ADDED:%U")
;; 	("b" "BtS" entry (file "~/Dropbox/org/refile.org")
;;          "* TODO %? \n ADDED:%U")
;; 	("m" "MHV" entry (file "~/Dropbox/org/refile.org")
;;          "* TODO %? \n ADDED:%U")
;; 	("p" "Projects" entry (file "~/Dropbox/org/refile.org")
;;          "* TODO %? \n ADDED:%U")
;; 	("n" "Notes" entry (file "~/Dropbox/org/notes.org")
;;          "* %?\n ADDED:%U")))

;; ;; Turns on fast state selection
;; (setq org-use-fast-todo-selection t)

;; ;; TODO sequences
;; (setq org-todo-keywords
;;        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; ; Use full outline paths for refile targets - we file directly with IDO
;; (setq org-refile-use-outline-path t)

;; ; Targets complete directly with IDO
;; (setq org-outline-path-complete-in-steps nil)

;; ; Allow refile to create parent tasks with confirmation
;; (setq org-refile-allow-creating-parent-nodes (quote confirm))

;; ;;;; Refile settings

;; ;; Refile stuff
;; ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; (setq org-refile-targets (quote ((nil :maxlevel . 9)
;;                                  (org-agenda-files :maxlevel . 9))))

;; ; Exclude DONE state tasks from refile targets
;; (defun bh/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'bh/verify-refile-target)

;; ;; Recursive events settings
;; (setq org-log-done t)
;; (setq org-log-repeat "time")

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(require 'helm-config)
(helm-mode 1)

;;;;;;;;;;;;
;; Aspell ;;
;;;;;;;;;;;;

(setq-default ispell-program-name "aspell")

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
