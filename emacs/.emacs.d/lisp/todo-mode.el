;;; todo-mode.el --- Major mode for my style of todo file

(defvar todo-mode-hook nil
  "Hook called by `'todo-mode'.")

(defvar todo-mode-map
  (let 
      ((todo-mode-map (make-keymap)))
    (define-key todo-mode-map "\C-j" 'newline-and-indent)
    todo-mode-map)
  "Keymap for todo major mode")

;; Font locking definitions. 
(defvar todo-heading-face         'todo-heading-face       "Face for top level headings")
(defvar todo-subheading-face      'todo-subheading-face    "Face for subheadings")
(defvar todo-subtask-face         'todo-subtask-face       "Face for subtasks")
(defvar todo-de_emphasize-face    'todo-de_emphasize-face  "Face for de-emphasize")
(defvar todo-plus-face            'todo-plus-face          "Face for +tags")
(defvar todo-at-face              'todo-at-face            "Face for @tags")
(defvar todo-bang-face            'todo-bang-face          "Face for !tags")
(defvar todo-pound-face           'todo-pound-face         "Face for #tags")

(defface todo-heading-face
  '((t (:foreground "LightGoldenrod1" )))
  "Face for top level headings")

(defface todo-subheading-face
  '((t (:foreground "LightGoldenrod3" )))
  "Face for subheadings")

(defface todo-subtask-face
  '((t (:foreground "LightGoldenrod4" )))
  "Face for subtasks")

(defface todo-de_emphasize-face
  '((t (:foreground "gray" )))
  "Face for de-emphasize")

(defface todo-plus-face
  '((t (:foreground "green4" )))
  "Face for +tags")

(defface todo-at-face
  '((t (:foreground "purple4" )))
  "Face for @tags")

(defface todo-bang-face
  '((t (:foreground "red4" )))
  "Face for !tags")

(defface todo-pound-face
  '((t (:foreground "cyan3" )))
  "Face for #tags")


(defconst todo-mode-font-lock-keywords
  (list
   '("\\~.*"               . todo-de_emphasize-face)  ;; temp - de-emphasize
   '("^[[:alnum:]].*::"    . todo-heading-face)
   '("   ?[[:alnum:]].*::" . todo-subheading-face)
   '("\\ ->.*"             . todo-subtask-face)
   '("\\+\\w*"             . todo-plus-face)
   '("\\@\\w*"             . todo-at-face)
   '("\\!\\w*"             . todo-bang-face)
   '("\\#\\w*"             . todo-pound-face)
   )
  "Font locking definitions for todo mode")

;; Custom syntax table
(defvar todo-mode-syntax-table (make-syntax-table) 
  "Syntax table for todo mode")

(modify-syntax-entry ?_  "w" todo-mode-syntax-table)  ; All _'s are part of words. 
(modify-syntax-entry ?-  "w" todo-mode-syntax-table)  ; All -'s are part of words.
(modify-syntax-entry ?:  "w" todo-mode-syntax-table)  ; All :'s are part of words.
(modify-syntax-entry ?\" "." todo-mode-syntax-table)  ; don't highlight strings

;; ENTRY point
(defun todo-mode  ()
  "Major mode for editing todo files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table todo-mode-syntax-table)
  (use-local-map todo-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(todo-mode-font-lock-keywords))
;;  (set (make-local-variable 'indent-line-function) 'cisco-router-indent-line)
;;  (set (make-local-variable 'comment-start) "!")
;;  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)!+ *")
  (setq major-mode 'todo-mode
	mode-name "Todo")
  (run-hooks todo-mode-hook))

(add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todo-mode))

(provide 'todo-mode)

;;; todo-mode.el ends here
