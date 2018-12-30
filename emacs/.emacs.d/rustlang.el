;; rustup component add rust-src
;; cargo install racer

(defun jr/rust-mode-hook ()
  (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))

(use-package rust-mode
  :ensure t
  :init   
  :config (add-hook 'rust-mode-hook 'jr/rust-mode-hook))

(use-package cargo
  :ensure t
  :init   (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :init   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t
  ;; :config (progn
  ;;           (setq racer-cmd "~/.cargo/bin/racer")
  ;;           (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
  :init (progn
          (add-hook 'rust-mode-hook #'racer-mode)
          (add-hook 'racer-mode-hook #'eldoc-mode)
          (add-hook 'racer-mode-hook #'company-mode)))
