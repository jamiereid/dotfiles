(require 'package)

;; Repos to use for packages
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; set up load-paths and autoloads for installed packages so we can configure
;; them and fetch available packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))  ; results in slow startup if enabled. but did the if help?

;; List of packages to ensure is installed
(setq my-package-list '(diminish
                        use-package))

;; Auto install my-package-list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

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


;;;; term stuff
; https://echosa.github.io/blog/2012/06/06/improving-ansi-term/  look here
(setq explicit-shell-file-name "/bin/bash")

; close buffer after quit
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

; is this required? (pasting into term)
;(eval-after-load "term"
;  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;;;; Custom Lisp
(load "jrr-lib.el")

(require 'ios-config-mode)
(require 'cisco-router-mode)
(require 'todo-mode)
(require 'emacs-surround)
(add-to-list 'emacs-surround-alist '("[" . ("[" . "]")))
(add-to-list 'emacs-surround-alist '("(" . ("(" . ")")))

;;;;  Tidy up GUI
(setq inhibit-splash-screen t)  ; No need for a splash screen
(menu-bar-mode -1)              ; No menu bar

;; Here we turn off some window system specific things; but of course, we only need to do that if we're in an X session.
(if (display-graphic-p)
    (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)))


;;;;
(setq-default explict-shell-file-name "/bin/bash") ; make sure we use bash
(show-paren-mode 1)  ; Highlight the matching paren (or other character) for the one under the cursor.
(set-face-background 'show-paren-match "#aaaaaa")
(set-face-attribute 'show-paren-match nil 
        :weight 'bold :underline nil :overline nil :slant 'normal)

(column-number-mode 1)        ; It can sometimes be useful to see the current column number in the mode line.
;(global-linum-mode t)       ; I almost always want line numbers, so let's turn that on
;(setq linum-format " %3i ") ; and make it a little more readable) for every buffer...
;(require 'linum-off)        ; and then use a custom bit of lisp from Matt Fidler's github to turn it off for
                             ; some buffers and modes (eg. *scratch* buffers and org-mode). We could customise
                             ; these modes using the linum-disabled-modes-list variable, but the defaults are good.

(global-visual-line-mode 1)   ; turn on word-wrapping and rebind C-a, C-e, and C-k to commands that operate on visual
                              ; lines instead of logical ones.

(setq-default indent-tabs-mode nil)  ; Spaces are the One and Only Way. setq-default is used to allow overriding using a local buffer variable.
(electric-pair-mode 1)               ; Magically insert a closing delimiter when an opening one is inserted.
(global-auto-revert-mode 1)          ; Reload files if changed on disk but not in the buffer.


;; scratch buffer message
(setq initial-scratch-message ";; walrus\n\n\n")

;; key bindings
(load-user-file "keybindings.el")


;;;; Packages
(require 'use-package)
;(load-user-file "evil.el")
(load-user-file "ivy-swiper-counsel.el")
(load-user-file "git.el")
(load-user-file "org-mode.el")
;(load-user-file "golang.el")
;(load-user-file "rustlang.el")
;(load-user-file "notmuch.el")

(use-package rainbow-mode  ; display the background of hex colors with the color they represent
  :ensure t
  :diminish (rainbow-mode . "")
  :config   (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package yaml-mode
  :ensure t
  :init   (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :config (add-hook 'yaml-mode-hook
                    '(lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package transpose-frame
  :ensure t)

(use-package highlight-numbers
  :ensure t
  :diminish ""
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))


;; load theme
(load-user-file "theme-naysayer.el")

;; Local overrides if exists (~/.emacs.d/local-overrides.el)
(if (file-readable-p (expand-file-name "local-overrides.el" user-init-dir))
    (load-file (expand-file-name "local-overrides.el" user-init-dir)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ox-gfm transport-frames zenburn-theme org-beautify-theme yaml-mode use-package rainbow-mode magit git-gutter-fringe diminish counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
