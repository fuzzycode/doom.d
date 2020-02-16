;; -*- lexical-binding: t; -*-

(defvar +org/org-directory "~/Documents/Org")

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

;; Add agenda files
(after! org
  (setq org-refile-targets (quote ((+org/todo-file :maxlevel . 2)
                                   (+org/notes-file :level . 2))))

  (when (file-exists-p +org/calendar-file)
    (add-to-list 'org-agenda-files +org/calendar-file))

  (when (file-exists-p +org/todo-file)
    (add-to-list 'org-agenda-files +org/todo-file)))

;; Functions
(defun +org/project-org-file-name ()
  (concat (string-remove-prefix "." (projectile-project-name)) ".org"))

(defun +org/projects-directory ()
  (concat org-directory "/projects/"))

(defun +org/project-org-file-path ()
  (concat (+org/projects-directory) (+org/project-org-file-name)))


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
            :desc "Archive File" :g "a" (lambda! () (find-file +org/archive-file))))
        (:prefix "p"
          :desc "Open Org File" :g "o" (lambda! () (find-file (+org/project-org-file-path))))))

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

(after! org
;;;###package
  (use-package! org-expiry
    :bind (:map org-mode-map
            ("C-c C-e" . #'org-expiry-insert-expiry))
    :config (setq org-expiry-inactive-timestamps t)
     (add-hook 'org-capture-before-finalize-hook #'+org/insert-creation)
     (add-hook 'org-insert-heading-hook #'+org/insert-creation)))

;;;###package
(use-package! doct
  :after org
  :bind (("C-c C-c" . #'org-capture))
  :init (setq org-capture-templates '())
  :config (add-to-list 'org-agenda-files (+org/projects-directory))
  (setq org-capture-templates
        (append org-capture-templates
                (doct '(("Tasks"
                         :keys "t"
                         :file +org/todo-file
                         :template "* %doct(todo) %^{Description}\n%?"
                         :children (("Task" :keys "t" :todo "TODO")
                                    ("Idea" :keys "i" :todo "IDEA")))
                        ("Project"
                         :keys "p"
                         :template "* %doct(todo) %?"
                         :file (lambda () (+org/project-org-file-path))
                         :contexts ((:function (lambda () (projectile-project-p (buffer-file-name (current-buffer))))))
                         :children (("Task" :keys "p" :todo "TODO" :headline "Tasks")
                                    ("Idea" :keys "i" :todo "IDEA" :headline "Tasks")
                                    ("Note" :keys "n" :template "* %?" :headline "Notes")
                                    ("Snippet" :keys "s" :headline "Notes" :template "* %^{Description}\nCaptured From: [[file:%F][%f]]\n#+BEGIN_SRC %^{Language}\n%i\n#+END_SRC\n%?")))
                        ("Feedback"
                         :keys "f"
                         :file +org/notes-file
                         :heading "Feedback"
                         :template "* %?")
                        ("Notes"
                         :keys "n"
                         :file +org/notes-file
                         :heading "Notes"
                         :template "* %?"))))))
