(use-package magit
  :ensure t
  :commands magit-git-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init (progn
          (use-package magit-blame
            :bind ("C-c C-g b" . magit-blame-mode))

          ;; Close popup when commiting - this stops the commit window hanging around
          ;; From: http://git.io/rPBE0Q
          (defadvice git-commit-commit (after delete-window activate)
            (delete-window))

          (defadvice git-commit-abort (after delete-window activate)
            (delete-window))

          ;; these two force a new line to be inserted into a commit window, which stops the invalid style showing up.
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
             )))


(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  :init (progn
          (setq git-gutter:separator-sign " "
                git-gutter:lighter " GG")))
