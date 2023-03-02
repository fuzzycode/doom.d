;; -*- lexical-binding: t; -*-

(setq org-directory (file-name-as-directory "~/Documents/Org"))
(setq org-roam-directory (file-name-as-directory (concat org-directory "roam/")))
(setq org-roam-dailies-directory (file-name-as-directory (concat org-roam-directory "dailies/")))

(defvar +org/archive-file (expand-file-name (concat (file-name-as-directory org-directory) "archive/archive.org")))

(after! org
  (setq org-hide-emphasis-markers t
        org-log-into-drawer t
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-treat-insert-todo-heading-as-state-change t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-archive-mark-done t
        org-todo-keywords '((sequence "TODO(t)" "WORKING(w)" "BLOCKED(b)" "IDEA(i)" "|" "DONE(d)" "CANCELED(c)" "DELEGATE(D)")
                           (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------")))


(after! org-roam
  (setq org-roam-v2-ack t)

  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target (file+head +bl/org-roam-file-format  "#+title: ${title}\n#+date: %U\n")
                                      :unnarrowed t)))


  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %? \n:PROPERTIES:\nCREATED: %<%T>\n:END:"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n"))))

  (add-hook 'find-file-hook #'+bl/org-roam-project-update-tag)
  (add-hook 'before-save-hook #'+bl/org-roam-project-update-tag))
