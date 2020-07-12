;;;; Key bindings

(global-unset-key (kbd "C-z"))                ; Be gone!
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c i") 'indent-region) ; indent regions of code without having to first mark it.

(global-set-key (kbd "C-x <up>")    'windmove-up) 
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x w s")     'window-swap-states)

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

(global-set-key [f8]  'goto-line)
(global-set-key [f12] 'eval-buffer)

(global-set-key (kbd "C-u") 'upcase-word)

(global-set-key (kbd "C-q") 'emacs-surround)
