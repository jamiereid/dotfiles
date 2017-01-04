;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(setq debug-on-error t)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

; Load modules
(setq org-modules (quote (org-habit)))

(setq org-directory "~/org/")
(setq org-agenda-files (quote ("~/org")))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Custom Key Bindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Todo
(setq org-log-done (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))
;;(setq org-use-fast-todo-selection t)
;;(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-default-notes-file (concat (file-name-as-directory org-directory) "refile.org"))

(setq org-capture-templates
      (quote (("t" "todo" entry (file (concat
                                       (file-name-as-directory org-directory)
                                       "refile.org"))
               "* TODO %?\n%U\n%a\n")
              ("n" "note" entry (file (concat
                                       (file-name-as-directory org-directory)
                                       "refile.org"))
               "* %? :NOTE:\n%U\n%a\n")
              ("j" "journal" entry (file+datetree (concat
                                                   (file-name-as-directory org-directory)
                                                   "journal.org"))
               "* %?\n%U\n")
              ("m" "meeting" entry (file (concat
                                          (file-name-as-directory org-directory)
                                          "refile.org"))
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("b" "media" checkitem (file (concat
                                            (file-name-as-directory org-directory)
                                            "refile.org"))
               "[ ] %?"))))

; Automatically place a blank line before a new heading or plain test list item
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

; Force mark all child tasks as done before parent can be]
(setq org-enforce-todo-dependencies t)

;; Refile settings
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-indirect-buffer-display 'current-window)

; Exclude DONE state tasks from refile targets
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Archive settings
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; Agenda Settings
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-habit)
                          (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

;; Habit settings
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(provide 'orgsettings)
;;; orgsettings.el ends here
