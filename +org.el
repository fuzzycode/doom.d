;; -*- lexical-binding: t; -*-

(setq org-directory (file-name-as-directory "~/Documents/Org"))
(setq org-roam-directory (file-name-as-directory (concat org-directory "roam/")))
(setq org-roam-dailies-directory (file-name-as-directory (concat org-roam-directory "dailies/")))

;; ;; Define my different files
(defvar +org/archive-file (expand-file-name (concat (file-name-as-directory org-directory) "archive/archive.org")))
(defvar +org/calendar-file (expand-file-name (concat (file-name-as-directory org-directory) "calendar.org")))

(after! org
  (setq org-hide-emphasis-markers t
        org-log-into-drawer t
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-treat-insert-todo-heading-as-state-change t
        org-agenda-include-diary t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-files (ensure-list (expand-file-name +org-capture-projects-file org-directory))
        org-archive-mark-done t
        org-todo-keywords '((sequence "TODO(t)" "WORKING(w)" "BLOCKED(b)" "IDEA(i)" "|" "DONE(d)" "CANCEL(c)" "DELEGATE(D)")
                           (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------")))


(after! org-roam
  (setq org-roam-v2-ack t)

  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:\n\n")
                                      :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %? \n:PROPERTIES:\nCREATED: %<%T>\n:END:"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n")))))


(after! org-capture
  (setq org-capture-templates (doct '(("Project"
                                       :keys "p"
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :contexts (:when (projectile-project-p))
                                       :children (("Task" :keys "t" :template "* TODO %?" :function +bl/capture-central-project-todo)
                                                  ("Idea" :keys "i" :template "* IDEA %?" :function +bl/capture-central-project-todo)
                                                  ("Note" :keys "n" :template "* %?" :function +bl/capture-central-project-notes)
                                                  ("Snippet" :keys "s" :template +bl/capture-snippet :contexts (:when (region-active-p)) :function +bl/capture-central-project-notes)))
                                      ("Note"
                                       :keys "n"
                                       :prepend nil
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :template "* %?"
                                       :file (lambda () (expand-file-name +org-capture-projects-file org-directory))
                                       :headline "General Notes")
                                      ("Task"
                                       :keys "t"
                                       :prepend nil
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :template "* TODO %?"
                                       :file (lambda () (expand-file-name +org-capture-projects-file org-directory))
                                       :headline "General Tasks")))))
