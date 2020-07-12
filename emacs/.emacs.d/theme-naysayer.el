;;;;
;;;; Naysayer Theme
;;;;

(global-font-lock-mode 1) ; Make sure that syntax highlighting is always on.

(set-frame-font     "Liberation Mono 11" nil t)
(add-to-list        'default-frame-alist '(font . "Liberation Mono 11"))
(set-face-attribute 'default t :font "Liberation Mono 11")
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
;(set-face-attribute 'todo-heading-face                nil :foreground "#FFFFFF")
;(set-face-attribute 'todo-subheading-face             nil :foreground "#C3C7B5")
;(set-face-attribute 'todo-subtask-face                nil :foreground "#676956")
;(set-face-attribute 'todo-de_emphasize-face           nil :foreground "#535b4c")
;(set-face-attribute 'todo-plus-face                   nil :foreground "#60D952")
;(set-face-attribute 'todo-at-face                     nil :foreground "#52C3A8")
;(set-face-attribute 'todo-bang-face                   nil :foreground "#CC7700")
;(set-face-attribute 'todo-pound-face                  nil :foreground "#9370DB")

;;org mode
(set-face-attribute 'org-document-title      nil :height 1.6 :foreground "#FFFFFF")
(set-face-attribute 'org-level-1             nil :height 1.1 :foreground "#FFFFFF")
(set-face-attribute 'org-level-2             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-3             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-4             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-5             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-6             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-7             nil             :foreground "#C3C7B5")
(set-face-attribute 'org-level-8             nil             :foreground "#C3C7B5")

(set-face-attribute 'org-block-begin-line    nil :foreground "#FFF" :background "#5E5E5E")
(set-face-attribute 'org-block               nil                       :background "#072627")
(set-face-attribute 'org-block-end-line      nil :foreground "#FFF" :background "#5E5E5E")

;(org-agenda-date-today-face      ((t (:foreground "white" :slant italic :weight bold))) t)
;(org-agenda-structure-face      ((t (:inherit font-lock-comment-face))))
;(org-archived-face ((t (:foreground ,zenburn-fg :weight bold))))
;(org-checkbox-face ((t (:background ,zenburn-bg+2 :foreground "white"                                    :box (:line-width 1 :style released-button)))))
;(org-date-face ((t (:foreground ,zenburn-blue :underline t))))
;(org-deadline-announce-face ((t (:foreground ,zenburn-red-1))))
;(org-done-face ((t (:bold t :weight bold :foreground ,zenburn-green+3))))
;(org-formula-face ((t (:foreground ,zenburn-yellow-2))))
;(org-headline-done-face ((t (:foreground ,zenburn-green+3))))
;(org-hide-face ((t (:foreground ,zenburn-bg-1))))
;(org-link-face ((t (:foreground ,zenburn-yellow-2 :underline t))))
;(org-scheduled-face ((t (:foreground ,zenburn-green+4))))
;(org-scheduled-previously-face ((t (:foreground ,zenburn-red-4))))
;(org-scheduled-today-face ((t (:foreground ,zenburn-blue+1))))
;(org-special-keyword-face ((t (:foreground ,zenburn-yellow-1))))
;(org-table-face ((t (:foreground ,zenburn-green+2))))
;(org-tag-face ((t (:bold t :weight bold))))
;(org-time-grid-face ((t (:foreground ,zenburn-orange))))
;(org-todo-face ((t (:bold t :foreground ,zenburn-red :weight bold))))
;(org-upcoming-deadline-face ((t (:inherit font-lock-keyword-face))))
;(org-warning-face ((t (:bold t :foreground ,zenburn-red :weight bold))))


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
