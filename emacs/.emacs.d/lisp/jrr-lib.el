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

(defun jrr/current-line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun jrr/subnets-to-org-table ()
  (interactive)
  (save-excursion
    (backward-paragraph)
    (newline)
    (insert "| Subnet | Name |")
    (next-line)
    (beginning-of-line)
    (insert "|-")
    (newline)
    (while (not (jrr/current-line-empty-p))
      (beginning-of-line)
      (insert "| ")
      (search-forward " ")
      (insert " | ")
      (end-of-line)
      (insert " |")
      (next-line)))
    (org-table-align))
