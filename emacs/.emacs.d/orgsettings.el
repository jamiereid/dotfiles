;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(setq debug-on-error t)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

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

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)
