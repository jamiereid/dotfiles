(use-package ivy
  :ensure t
  :diminish t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t   ; adds recent files and bookmarks to the ivy-switch-buffer.
                  ivy-count-format "%d/%d ")) ; display current and total number in the collection in the prompt.
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
