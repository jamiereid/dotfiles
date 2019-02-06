(require 'package)

;; Repos to use for packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; List of packages to ensure is installed
(setq package-list '(diminish
		     use-package))

;; set up load-paths and autoloads for installed packages so we can configure
;; them and fetch available packages
(package-initialize)
;;(package-refresh-contents)  ; results in slow startup if enabled.

(require 'use-package)

;; Set up user-emacs-directory that we use in other places.
(defconst user-init-dir
            (cond ((boundp 'user-emacs-directory)
                            user-emacs-directory)
                          ((boundp 'user-init-directory)
                                    user-init-directory)
                          (t "~/.emacs.d/")))

;; I have some custom lisp functions I like to use. The following line makes
;; sure my ~/.emacs.d/lisp directory is part of the load path.
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; emacs likes to leave it's droppings in the working directory -- what a mess!
(setq backup-directory-alist
    `((",*" . ,"~/.emacs-tmp/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-tmp/" t)))


;;;;
;;;; Custom Lisp
;;;;

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;;
;;;;  Tidy up GUI
;;;;

(setq inhibit-splash-screen t)  ; No need for a splash screen
(menu-bar-mode -1)              ; No menu bar - it just takes up screen real
                                ; estate and serves as a distraction.

;; Here we turn off some window system specific things; but of course, we only
;; need to do that if we're running in a window system such as X.
(if (display-graphic-p)
    (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)))

;;;;
;;;;
;;;;

;; Highlight the matching paren (or other character) for the one under the cursor.
(show-paren-mode 1)
(set-face-background 'show-paren-match "#aaaaaa")
(set-face-attribute 'show-paren-match nil 
        :weight 'bold :underline nil :overline nil :slant 'normal)

(column-number-mode 1)        ; It can sometimes be useful to see the current
                              ; column number in the mode line.
;;(global-linum-mode t)       ; I almost always want line numbers, so let's turn that on
;;(setq linum-format " %3i ") ; and make it a little more readable) for every buffer...
;;(require 'linum-off)        ; and then use a custom bit of lisp from Matt
                              ; Fidler's github to turn it off for some buffers
                              ; and modes (eg. *scratch* buffers and
                              ; org-mode). We could customise these modes using
                              ; the linum-disabled-modes-list variable, but the
                              ; defaults are good.

(global-visual-line-mode 1)   ; turn on word-wrapping and rebind C-a, C-e, and
                              ; C-k to commands that operate on visual lines
                              ; instead of logical ones.

(setq-default indent-tabs-mode nil)  ; Spaces are the One and Only Way.
                                     ; setq-default is used to allow
                                     ; overriding using a local buffer variable.
(electric-pair-mode 1)               ; Magically insert a closing delimiter when
                                     ; an opening one is inserted.
(global-auto-revert-mode 1)          ; Reload files if changed on disk but not in
                                        ; the buffer.

;;;;
;;;; Key bindings
;;;;

(global-unset-key (kbd "C-z"))                ; Be gone!
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c i") 'indent-region) ; indent regions of code without
                                              ; having to first mark it.

;; Set up some nice keybindings for changing the size of windows.
(global-set-key (kbd "C-x <up>") 'shrink-window)
(global-set-key (kbd "C-x <down>") 'enlarge-window)
(global-set-key (kbd "C-x <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x w s") 'window-swap-states)

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

(global-set-key [f8]  'goto-line)
(global-set-key [f12] 'eval-buffer)

(global-set-key (kbd "C-u") 'upcase-word)


;;;;
;;;; Packages
;;;;

(use-package ivy
  :ensure t
  :diminish t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t   ; adds recent files and bookmarks
                                              ; to the ivy-switch-buffer.
                  ivy-count-format "%d/%d ")) ; display current and total number
                                              ; in the collection in the prompt.
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package counsel
  :ensure t
  :diminish counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("C-c k" . counsel-ag)))

(use-package swiper
  :ensure t
;  :bind (("C-s" . swiper))
  :config
  (progn
    (setq ivy-initial-inputs-alist nil
          ivy-use-virtual-buffers t
          ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    (ivy-mode 1)))

(use-package rainbow-mode  ; display the background of hex colors with the color
                           ; they represent
  :ensure t
  :diminish (rainbow-mode . "")
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.css\\'"
         "\\.php\\'")
  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (require 'yasnippet)
    (yas-global-mode 1)))

(use-package highlight-numbers
  :ensure t
  :diminish ""
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package projectile
  :ensure t
  :diminish ""
  :config
  (progn
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))
                     
(use-package neotree
  :ensure t
  :diminish ""
  :config
  (global-set-key [f7] 'neotree-toggle))

(use-package spaceline
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme))

;;;;
;;;; csharp
;;;;

(use-package csharp-mode
	:ensure t)

(use-package flycheck
	:ensure t
	:hook (csharp-mode . (lambda () (flycheck-mode 1))))

(use-package omnisharp
	:ensure t
	:after csharp-mode
	:hook (csharp-mode . (lambda () (omnisharp-mode 1))))

(use-package company
  :ensure t
  :diminish ""
  :config
	(global-company-mode)
	(add-to-list 'company-backends 'company-omnisharp))

;;;
;;; magit & git-gutter-fringe
;;;

(use-package magit
  :ensure t
  :commands magit-git-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init (progn
          (use-package magit-blame
            :bind ("C-c C-g b" . magit-blame-mode))

          ;; Close popup when commiting - this stops the commit window
          ;; hanging around
          ;; From: http://git.io/rPBE0Q
          (defadvice git-commit-commit (after delete-window activate)
            (delete-window))

          (defadvice git-commit-abort (after delete-window activate)
            (delete-window))

          ;; these two force a new line to be inserted into a commit window,
          ;; which stops the invalid style showing up.
          ;; From: http://git.io/rPBE0Q
          (defun magit-commit-mode-init ()
            (when (looking-at "\n")
              (open-line 1)))

          (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config (progn
            (setq
             magit-completing-read-function 'magit-ido-completing-read                      ; use ido to look for branches
             magit-default-tracking-name-function 'magit-default-tracking-name-branch-only  ; don't put "origin-" in front of new branch names by default
             magit-status-buffer-switch-function 'switch-to-buffer                          ; open magit status in same window as current buffer
             magit-diff-refine-hunk t                                                       ; highlight word/letter changes in hunk diffs
             magit-rewrite-inclusive 'ask                                                   ; ask me if I want to include a revision when rewriting
             magit-save-some-buffers t                                                      ; ask me to save buffers
             magit-process-popup-time 10                                                    ; pop the process buffer if we're taking a while to complete
             magit-set-upstream-on-push 'askifnotset                                        ; ask me if I want a tracking upstream
             ))
  )


(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  :init (progn
          (setq git-gutter:separator-sign " "
                git-gutter:lighter " GG")))

;;;;
;;;; org mode
;;;;

;(load-user-file "orgmode.el")
(load-user-file "golang.el")
;(load-user-file "rustlang.el")
(load-user-file "notmuch.el")

;;;;
;;;; Custom lisp loads
;;;;

(require 'ios-config-mode)
(require 'cisco-router-mode)
(require 'todo-mode)

;;;;
;;;; Theme
;;;;

(global-font-lock-mode 1) ; Make sure that syntax highlighting is always on.

(set-frame-font     "Liberation Mono 8" nil t)
(add-to-list        'default-frame-alist '(font . "Liberation Mono 8"))
(set-face-attribute 'default t :font "Liberation Mono 8")
(fringe-mode        '(15 . 0))  ; set fringe sizes


(add-to-list 'default-frame-alist '(tty-color-mode . -1)) ; don't use colours in terminal mode

;; gradient from default fore to back
;; #C7AC85, #B39E7B, #A09172, #8D8368, #7A765F, #676956, #535B4C, #404E43, #2D4039, #1A3330, #072627
;; #60D952, #57C74D, #4EB549, #45A345, #3C9140, #337F3C, #2A6D38, #215B33, #18492F, #0F372B, #072627
;; #9370DB, #8568C9, #7761B7, #6959A5, #5B5293, #4D4B81, #3F436F, #313C5D, #23344B, #152D39, #072627
;; #CC7700, #B86E03, #A46607, #905E0B, #7D560F, #694E13, #554617, #423E1B, #2E361F, #1A2E23, #072627

(set-face-attribute 'default                          nil :foreground "#c7ac85"    :background "#072627")
(set-face-attribute 'cursor                           nil                          :background "#91EC93")
(set-face-attribute 'font-lock-builtin-face           nil :foreground "lightgreen")
(set-face-attribute 'font-lock-keyword-face           nil :foreground "#FFFFFF")
(set-face-attribute 'font-lock-comment-face           nil :foreground "#60D952")
(set-face-attribute 'font-lock-doc-face               nil :foreground "#60D952")
(set-face-attribute 'font-lock-constant-face          nil :foreground "#52C3A8")
(set-face-attribute 'font-lock-string-face            nil :foreground "#52C3A8")
(set-face-attribute 'font-lock-warning-face           nil :foreground "#52C3A8")
(set-face-attribute 'font-lock-function-name-face     nil :foreground "#C3C7B5")
(set-face-attribute 'font-lock-type-face              nil :foreground "#C3C7B5")
(set-face-attribute 'font-lock-variable-name-face     nil :foreground "burlywood2")
(set-face-attribute 'mode-line                        nil :foreground "#072627"    :background "#c7ac85")
(set-face-attribute 'mode-line-inactive               nil :foreground "#5E5E5E"    :background "#AEAEAE")
(set-face-attribute 'vertical-border                  nil :foreground "#AEAEAE")
(set-face-attribute 'fringe                           nil                          :background "#072627")
(set-face-attribute 'region                           nil                          :background "#5E5E5E")

;; git-gutter-fringe
(set-face-attribute 'git-gutter-fr:modified           nil :foreground "#3F436F")
(set-face-attribute 'git-gutter-fr:added              nil :foreground "#2A6D38")
(set-face-attribute 'git-gutter-fr:deleted            nil :foreground "#905E0B")

;; todo-mode
(set-face-attribute 'todo-heading-face                nil :foreground "#FFFFFF")
(set-face-attribute 'todo-subheading-face             nil :foreground "#C3C7B5")
(set-face-attribute 'todo-subtask-face                nil :foreground "#676956")
(set-face-attribute 'todo-de_emphasize-face           nil :foreground "#535b4c")
(set-face-attribute 'todo-plus-face                   nil :foreground "#60D952")
(set-face-attribute 'todo-at-face                     nil :foreground "#52C3A8")
(set-face-attribute 'todo-bang-face                   nil :foreground "#CC7700")
(set-face-attribute 'todo-pound-face                  nil :foreground "#9370DB")

(with-eval-after-load 'highlight-numbers
  (set-face-attribute 'highlight-numbers-number       nil :foreground "cyan3"))

(add-hook 'todo-mode-hook 'highlight-numbers-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                          '(("\\(?:^\\|\\s-\\)\\(@\\(Todo\\|Incomplete\\|Cleanup\\|Factor\\|Robustness\\|Hardcoded\\)\\)" 1 '(:foreground "#CC7700") prepend)
                            ("\\(?:^\\|\\s-\\)\\(@Note\\)"                                                                1 '(:foreground "#AEAEAE") prepend)
                            ("\\(?:^\\|\\s-\\)\\(@\\(Broken\\|BROKEN\\|Hack\\|Bug\\)\\!*\\)"                              1 '(:foreground "#AA0000") prepend)))))

;;; indicate lines longer than 80 in prog-modes
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;;; font ligatures
;(set-language-environment "UTF-8")
;(set-default-coding-systems 'utf-8)
;(load-user-file "ligatures.el")

;;; scratch buffer
;(setq initial-major-mode 'org-mode)
(setq initial-scratch-message ";; walrus\n\n\n")


;;;;
;;;; local overrides if exists (~/.emacs.d/local-overrides.el)
;;;;

(if (file-readable-p (expand-file-name "local-overrides.el" user-init-dir))
    (load-file (expand-file-name "local-overrides.el" user-init-dir)))

;;;;
;;;; emacs likes to add things to our config :(
;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" default)))
 '(package-selected-packages
   (quote
    (doom-themes doom-modeline all-the-icons projectile neotree org-bullets racer flycheck-rust cargo rust-mode git-gutter-fringe magit org-pdfview zenburn-theme csharp-mode gruvbox-theme use-package diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

