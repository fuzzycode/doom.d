;; -*- lexical-binding: t; -*-

(defvar +org/org-directory "~/Documents/Org")

(defvar +org/created-property-string ":PROPERTIES:\n:CREATED: %U\n:END:")

(after! org
  (setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-log-into-drawer t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-treat-insert-todo-heading-as-state-change t

      org-archive-location (format "%s::%s" +org/archive-file "* From %s" )

      org-refile-target-verify-function #'+org/verify-refile-target

      org-directory +org/org-directory
      org-id-method 'uuidgen
      org-clone-delete-id t
      org-timeline-show-empty-dates t
      org-enforce-todo-dependencies t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-inhibit-startup t
      org-capture-templates '()
      org-confirm-babel-evaluate nil
      org-insert-heading-respect-content t
      org-archive-mark-done t

      org-return-follows-link t

      org-highest-priority ?A
      org-lowest-priority ?E
      org-default-priority ?C

      org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------")

      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-refile-allow-creating-parent-nodes 'confirm
      org-outline-path-complete-in-steps nil
      org-todo-keywords '((sequence "IDEA(i!)" "TODO(t!)" "IN-PROGRESS(p!)"  "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@/!)"))))

(set-popup-rule! "^\\*org" :side 'right :size 80 :select nil :modeline t)


;; Define my different files
(defvar +org/todo-file (concat (file-name-as-directory +org/org-directory) "todo.org"))
(defvar +org/notes-file (concat (file-name-as-directory +org/org-directory) "notes.org"))
(defvar +org/archive-file (concat (file-name-as-directory +org/org-directory) "archive.org"))
(defvar +org/inbox-file (concat (file-name-as-directory +org/org-directory) "inbox.org"))
(defvar +org/calendar-file (concat (file-name-as-directory +org/org-directory) "calendar.org"))
(defvar +org/projects-file (concat (file-name-as-directory +org/org-directory) "projects.org"))

;; Add agenda files
(after! org
  (setq org-refile-targets (quote ((+org/todo-file :maxlevel . 2)
                                   (+org/notes-file :level . 2))))

  (when (file-exists-p +org/calendar-file)
    (add-to-list 'org-agenda-files +org/calendar-file))

  (when (file-exists-p +org/todo-file)
    (add-to-list 'org-agenda-files +org/todo-file)))


(map! (:leader
        (:prefix ("a" . "applications")
          (:prefix ("o" . "org")
            :desc "Agenda" :g "a" #'org-agenda
            :desc "Capture" :g "c" #'org-capture
            :desc "Todo List" :g "t" #'org-todo-list))
        (:prefix "s"
          :desc "Search Org Directory" :g "o" #'+default/org-notes-search)
        (:prefix "f"
          (:prefix ("o" . "org")
            :desc "Browse Org files" :g "b" #'+default/browse-notes
            :desc "Todo File" :g "t" (lambda! () (find-file +org/todo-file))
            :desc "Notes File" :g "n" (lambda! () (find-file +org/notes-file))
            :desc "Calendar File" :g "c" (lambda! () (find-file +org/calendar-file))
            :desc "Inbox File" :g "i" (lambda! () (find-file +org/inbox-file))
            :desc "Archive File" :g "a" (lambda! () (find-file +org/archive-file))
            :desc "Projects File" :g "p" (lambda! () (find-file +org/projects-file))))))

;;;###package
(use-package! demo-it
  :after org)

;;;###package
(use-package! idle-org-agenda
  :defer 5
  :config (idle-org-agenda-mode))

;;;###package
(use-package! org-super-agenda
  :after (org org-agenda)
  :config (shut-up (org-super-agenda-mode))
  :init
  (setq org-super-agenda-groups
        '((:name "Schedule" :time-grid t :order 4)
          (:name "Today" :scheduled today :order 3)
          (:name "Important" :priority "A" :order 0)
          (:name "Overdue" :deadline past :order 2)
          (:name "Due soon" :deadline future :order 5)
          (:name "Todo" :todo "TODO" :order 7)
          (:name "In Progress" :todo "IN-PROGRESS" :order 6)
          (:name "Due today" :deadline today :order 1))))

;;;###package
(use-package! org-bullets
  :hook (org-mode . org-bullets-mode))

;;;###package
(use-package! org-tempo ;; for <s templates etc.
  :after org)

;;;###package
(use-package! doct
  :after org
  :init (setq org-capture-templates '())
  :config (setq org-capture-templates
                (append org-capture-templates
                        (doct '(("Tasks"
                                 :keys "t"
                                 :file +org/todo-file
                                 :template (lambda () (format "* %s %%^{description}\n%s\n%%?" "%doct(todo)" +org/created-property-string))
                                 :children (("Task" :keys "t" :todo "TODO")
                                            ("Idea" :keys "i" :todo "IDEA")))
                                ("Feedback"
                                 :keys "f"
                                 :file +org/notes-file
                                 :heading "Feedback"
                                 :template (lambda () (format "* %%? \n%s" +org/created-property-string)))
                                ("Notes"
                                 :keys "n"
                                 :file +org/notes-file
                                 :heading "Notes"
                                 :template (lambda () (format "* %%?\n%s" +org/created-property-string))))))))

;;;###package
(use-package! org-projectile
  :after (org projectile)
  :bind (("C-c c" . #'org-capture))
  :init (setq org-projectile-capture-template (format "%s\n%s" "* TODO %?" +org/created-property-string)
              org-link-elisp-confirm-function nil
              org-projectile-projects-file +org/projects-file)
  (map! :leader (:prefix ("p" . "project") :desc "Project Todo" :g "r" #'org-projectile-project-todo-completing-read))
  :config
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (add-to-list 'org-capture-templates (org-projectile-project-todo-entry
                                       :capture-character "p"))
  (add-to-list 'org-capture-templates (org-projectile-project-todo-entry
                                       :capture-heading "Project Idea"
                                       :capture-template (format "%s\n%s" "* IDEA %?" +org/created-property-string)
                                       :capture-character "P")))
