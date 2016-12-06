;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(setq debug-on-error t)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)
(require 'org)

(setq org-dir "~/org/")
;;(setq org-directory org-dir)
(setq org-agenda-files (quote (org-dir)))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Custom Key Bindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Todo
(setq org-log-done 'time)
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


(setq org-default-notes-file (concat org-dir "refile.org"))

;;(setq org-capture-templates
      ;; (quote (("t" "todo" entry (file (concat org-dir "refile.org"))
      ;;          "* TODO %?\n%U\n%a\n")
      ;;         ("n" "note" entry (file (concat org-dir "refile.org"))
      ;;          "* %? :NOTE:\n%U\n%a\n")
      ;;         ("j" "journal" entry (file+datetree (concat org-dir "journal.org"))
      ;;          "* %?\n%U\n")
      ;;         ("m" "meeting" entry (file (concat org-dir "refile.org"))
      ;;          "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
      ;;         ("b" "media" checkitem (file (concat org-dir "refile.org"))
      ;;          "[ ] %?"))))

