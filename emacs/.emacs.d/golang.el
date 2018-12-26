;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/rogpeppe/godef/
;; go get -u github.com/nsf/gocode

(defun jr/go-mode-hook ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'company-mode)
  (set (make-local-variable 'company-backends) '(company-go))
  (set (make-local-variable 'company-idle-delay) 0))

(use-package go-mode
  :ensure t
  :init   (setq gofmt-command "goimports")
  :config (add-hook 'go-mode-hook 'jr/go-mode-hook))

(use-package go-eldoc
  :ensure t
  :defer
  :init   (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :ensure t
  :defer  t
  :init   (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-go)))
