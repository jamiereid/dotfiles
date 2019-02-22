(setq debug-on-error 't)

(use-package org
  :ensure t
  :init (setq org-src-fontify-natively t     ; pretty code blocks
              org-src-tab-acts-natively t)
  :config (use-package org-pdfview
              :ensure t))

;; @Todo: Find out if org-bullets slowness has been fixed
(use-package org-bullets
 :ensure t
  :commands (org-bullets-mode)
  :after org
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(load-user-file "org-custom-lisp.el")

;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; (setq org-agenda-files '("~/org/gtd/inbox.org"
;;                          "~/org/gtd/standalone.org"
;;                          "~/org/gtd/tickler.org"
;;                          "~/org/gtd/projects.org"
;;                          "~/org/gtd/someday.org"))

;; (setq org-refile-targets '(("~/org/gtd/projects.org" :maxlevel . 2)
;;                            ("~/org/gtd/standalone.org" :level . 1)
;;                            ("~/org/gtd/someday.org"  :level . 2)
;;                            ("~/org/gtd/tickler.org"  :maxlevel . 2)))

;; (setq org-refile-allow-creating-parent-nodes 'confirm)  ; Allow refile to create parent tasks with confirmation

;; ;;;;
;; ;;;; Keybindings
;; ;;;;

;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ct" 'air-pop-to-org-agenda)

;; ;;;;
;; ;;;; Todo
;; ;;;;

;; (setq org-log-done       (quote time))
;; (setq org-log-redeadline (quote time))
;; (setq org-log-reschedule (quote time))

;; (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "MEETING(m)" "|" "INACTIVE(i@/!)" "DONE(d)" "CANCELLED(c@/!)")))
;; ;; (setq org-todo-keyword-faces
;; ;;       '(("TODO" :foreground "red" :weight bold)
;; ;; 	("NEXT" :foreground "blue" :weight bold)
;; ;; 	("DONE" :foreground "forest green" :weight bold)
;; ;; 	("WAITING" :foreground "orange" :weight bold)
;; ;; 	("INACTIVE" :foreground "magenta" :weight bold)
;; ;; 	("CANCELLED" :foreground "forest green" :weight bold)
;; ;;         ("MEETING" :foreground "forest green" :weight bold)))
;; ;; (setq org-todo-state-tags-triggers
;; ;;       '(("CANCELLED" ("CANCELLED" . t))
;; ;; 	("WAITING" ("WAITING" . t))
;; ;; 	("INACTIVE" ("WAITING") ("INACTIVE" . t))
;; ;; 	(done ("WAITING") ("INACTIVE"))
;; ;; 	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
;; ;; 	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
;; ;;         ("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))

;; (setq org-capture-templates
;;       (quote (("t" "Todo [inbox]" entry
;;                (file+headline "~/org/gtd/inbox.org" "Tasks")
;;                "* TODO %^{task} %^g\n%?\nAdded: %U")
;;               ("T" "Tickler" entry
;;                (file+headline "~/org/gtd/tickler.org" "Tickler")
;;                "* %i%? \nAdded:%U")
;;               ("n" "Next Task" entry
;;                (file+headline "~/org/gtd/inbox.org" "Tasks")
;;                "* NEXT %? \nAdded: %U \nDEADLINE: %t"))))

;; ;; (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))  ; Automatically place a blank line before a new heading or plain test list item
;; (setq org-enforce-todo-dependencies t)  ; Force mark all child tasks as done before parent can be
;; (setq org-use-fast-todo-selection t)

;; ;;;;
;; ;;;; Agenda
;; ;;;;

;; (setq org-enforce-todo-dependencies t)
;; (setq org-agenda-inhibit-startup nil)
;; (setq org-agenda-dim-blocked-tasks nil)
;; (setq org-agenda-compact-blocks nil)

;; ;; Variables for ignoring tasks with deadlines
;; (defvar gs/hide-deadline-next-tasks t)
;; (setq org-agenda-tags-todo-honor-ignore-options t)
;; (setq org-deadline-warning-days 10)

;; (setq org-agenda-custom-commands
;;       '(("h" "Habits" agenda "STYLE=\"habit\""
;; 	 ((org-agenda-overriding-header "Habits")
;; 	  (org-agenda-sorting-strategy
;; 	   '(todo-state-down effort-up category-keep))))
;; 	(" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
;; 					    (org-agenda-span 'day)
;; 					    (org-agenda-ndays 1)
;; 					    (org-agenda-start-on-weekday nil)
;; 					    (org-agenda-start-day "+0d")
;; 					    (org-agenda-todo-ignore-deadlines nil)))
;; 				(tags-todo "-INACTIVE-CANCELLED-ARCHIVE/!NEXT"
;; 					   ((org-agenda-overriding-header "Next Tasks:")
;; 					    ))
;; 				(tags "REFILE-ARCHIVE-REFILE=\"nil\""
;; 				      ((org-agenda-overriding-header "Tasks to Refile:")
;; 				       (org-tags-match-list-sublevels nil)))
;; 				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
;; 					   ((org-agenda-overriding-header "Active Projects:")
;; 					    (org-agenda-skip-function 'gs/select-projects)a))
;; 				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
;; 					   ((org-agenda-overriding-header "Standalone Tasks:")
;; 					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
;; 				;; (agenda "" ((org-agenda-overriding-header "Week At A Glance:")
;; 				;; 	    (org-agenda-ndays 5)
;; 				;; 	    (org-agenda-start-day "+1d")
;; 				;; 	    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
;; 				;; 	    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
;; 				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVE/!-NEXT"
;; 					   ((org-agenda-overriding-header "Remaining Project Tasks:")
;; 					    (org-agenda-skip-function 'gs/select-project-tasks)))
;; 				(tags "INACTIVE-ARCHIVE"
;; 				      ((org-agenda-overriding-header "Inactive Projects and Tasks")
;; 				       (org-tags-match-list-sublevels nil)))
;; 				(tags "ENDOFAGENDA"
;; 				      ((org-agenda-overriding-header "End of Agenda")
;; 				       (org-tags-match-list-sublevels nil))))
;; 	 ((org-agenda-start-with-log-mode t)
;; 	  (org-agenda-log-mode-items '(clock))
;; 	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
;; 				      (timeline . "  % s")
;; 				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
;; 				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
;; 				      (search . "  %i %-12:c")))
;; 	  (org-agenda-todo-ignore-deadlines 'near)
;; 	  (org-agenda-todo-ignore-scheduled t)))
;; 	("X" "Agenda" ((agenda "") (alltodo))
;; 	 ((org-agenda-ndays 10)
;; 	  (org-agenda-start-on-weekday nil)
;; 	  (org-agenda-start-day "-1d")
;; 	  (org-agenda-start-with-log-mode t)
;; 	  (org-agenda-log-mode-items '(closed clock state)))
;;          )))
