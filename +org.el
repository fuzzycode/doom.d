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
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-archive-mark-done t
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
  (setq org-capture-templates (doct '(("Tasks"
                                       :keys "t"
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :file (lambda () (+org-capture-todo-file))
                                       :children (("Task" :keys "t" :template "* TODO %?")
                                                  ("Task (Today)" :keys "d" :template "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"\")>")
                                                  ("Task (Tomorrow)" :keys "D" :template "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"+1\")>")
                                                  ("Reminder" :keys "r" :template "* TODO %?\nSCHEDULED: %^t")))
                                      ("Project"
                                       :keys "p"
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :template "* %{todo} %?"
                                       :contexts ((:function (lambda () (projectile-project-p (buffer-file-name (current-buffer))))))
                                       :children (("Task" :keys "p" :todo "TODO" :headline "Tasks" :file (lambda () (+org-capture-project-todo-file)))
                                                  ("Note" :keys "n" :template "* %?" :headline "Notes"  :file (lambda () (+org-capture-project-notes-file)))
                                                  ("Snippet" :keys "s" :headline "Notes" :template +bl/capture-snippet :contexts (:when (region-active-p)))) :file (lambda () (+org-capture-project-notes-file)))
                                      ("Centralized Project"
                                       :keys "c"
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :template "* %{todo} %?"
                                       :children (("Task" :keys "p" :todo "TODO" :headline "Tasks" :file (lambda () (+org-capture-central-project-todo-file)))
                                                  ("Note" :keys "n" :template "* %?" :headline "Notes"  :file (lambda () (+org-capture-central-project-notes-file)))
                                                  ("Snippet" :keys "s" :headline "Notes" :template +bl/capture-snippet :contexts (:when (region-active-p)))) :file (lambda () (+org-capture-central-project-notes-file)))
                                      ("Feedback"
                                       :keys "f"
                                       :file (lambda () (+org-capture-notes-file))
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :headline "Feedback"
                                       :template "* %?")
                                      ("Notes"
                                       :keys "n"
                                       :before-finalize (lambda () (+bl/insert-creation))
                                       :file (lambda () (+org-capture-notes-file))
                                       :headline "Note"
                                       :template "* %?")
                                      ))))
