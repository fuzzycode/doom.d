;; -*- lexical-binding: t; -*-

(setq org-directory (file-name-as-directory "~/Documents/Org"))

(after! org
  (setq org-src-preserve-indentation t
        org-src-fontify-natively t
        org-log-into-drawer t
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-treat-insert-todo-heading-as-state-change t
        org-hide-emphasis-markers t
        org-refile-target-verify-function #'+org/verify-refile-target

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

        org-id-track-globally t
        org-id-method 'uuidgen

        org-return-follows-link t

        org-highest-priority ?A
        org-lowest-priority ?E
        org-default-priority ?C

        org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------")

        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes t
        org-refile-allow-creating-parent-nodes 'confirm
        org-outline-path-complete-in-steps nil
        org-todo-keywords '((sequence "IDEA(i!)" "TODO(t!)" "IN-PROGRESS(p!)"  "BLOCKED(b@/!)" "|" "DONE(d!)")
                            (sequence "|" "CANCELED(c@/!)" "EXPIRED(!)")
                            (sequence "PENDING(h)" "|" "COMPLETED(c!)"))
        org-todo-keyword-faces '(("IDEA" . +org-todo-active)
                                 ("TODO" . +org-todo-active)
                                 ("PENDING" . +org-todo-active)
                                 ("IN-PROGRESS" . +org-todo-active)
                                 ("BLOCKED" . +org-todo-onhold)
                                 ("DONE" . +org-todo-onhold)
                                 ("CANCELED" . +org-todo-onhold)
                                 ("EXPIRED" . +org-todo-onhold)))


  (after! recentf
    (add-to-list 'recentf-exclude org-id-locations-file)))

;; (set-popup-rule! "^\\*org" :side 'right :size 80 :select nil :modeline t)


;; Define my different files
(defvar +org/todo-file (expand-file-name (concat (file-name-as-directory org-directory) "todo.org")))
(defvar +org/notes-file (expand-file-name (concat (file-name-as-directory org-directory) "notes.org")))
(defvar +org/archive-file (expand-file-name (concat (file-name-as-directory org-directory) "archive.org")))
(defvar +org/inbox-file (expand-file-name (concat (file-name-as-directory org-directory) "inbox.org")))
(defvar +org/calendar-file (expand-file-name (concat (file-name-as-directory org-directory) "calendar.org")))
(defvar +org/journal-file (expand-file-name (concat (file-name-as-directory org-directory) "journal.org")))

(setq org-archive-location (format "%s::%s" +org/archive-file "* From %s" ))

;; Add agenda files
(after! org
  (when (file-exists-p +org/calendar-file)
    (add-to-list 'org-agenda-files +org/calendar-file))

  (when (file-exists-p +org/todo-file)
    (add-to-list 'org-agenda-files +org/todo-file)))


(after! ox-latex
  (add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;;###package
(use-package! demo-it
  :after org)

;;;###package
(use-package! org-super-agenda
  :after (org org-agenda)
  :init (setq org-super-agenda-groups '((:name "Today"
                                         :scheduled today)
                                        (:name "Overdue"
                                         :scheduled past)
                                        (:name "Soon"
                                         :scheduled future)))
  (setq org-agenda-custom-commands
        '(("d" "Daily Planner"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-start-day nil)
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-skip-deadline-if-done nil)
                        (org-super-agenda-groups
                         '((:name "Habits"
                            :habit t)
                           (:name "Tasks"
                            :deadline today
                            :scheduled today)
                           (:name "Due Soon"
                            :deadline future
                            :scheduled future)
                           (:name "Overdue"
                            :deadline past
                            :scheduled past)
                           (:name "Schedule"
                            :time-grid t)))))))))
  :config
  ;TODO This clears the whole map and is too invasive. Used to not break j/k bindings in agenda buffer
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (shut-up (org-super-agenda-mode)))

;;;###package
(use-package! org-expiry
  :after org
  :commands org-expiry-insert-expiry
  :bind (:map org-mode-map
         ("C-c C-e" . #'org-expiry-insert-expiry))
  :config (setq org-expiry-inactive-timestamps t))

;;;###package
(use-package! org-ql
  :defer t
  :commands org-ql-search
  :init
   (set-popup-rule! "^\\*Org QL View:" :side 'bottom :size .5 :select t :quit 'current)
   (map! (:leader (:prefix "o"
                        :desc "Org QL Search" :ng "q" #'org-ql-search))))

;;;###package
(use-package! doct
  :after org-capture
  :bind (("C-c c" . #'org-capture))
  :init (setq org-capture-templates '())
  :config
  (add-to-list 'org-agenda-files (+org/projects-directory))
  (setq org-capture-templates
        (append org-capture-templates
                (doct '(("Tasks"
                         :keys "t"
                         :before-finalize (lambda () (+org/insert-creation))
                         :file +org/todo-file
                         :template "* %{todo} %?"
                         :children (("Task" :keys "t" :todo "TODO")
                                    ("Task (Today)" :keys "d" :template "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"\")>")
                                    ("Task (Tomorrow)" :keys "D" :template "* TODO %?\nSCHEDULED: <%(org-read-date nil nil \"+1\")>")
                                    ("Idea" :keys "i" :todo "IDEA")
                                    ("Reminder" :keys "r" :template "* TODO %?\nSCHEDULED: %^t")))
                        ("Journal"
                         :keys "j"
                         :file +org/journal-file
                         :datetree t
                         :children (("Timed Entry" :keys "t" :template "* %(format-time-string \"\%H:\%M\") - %?")))
                        ("Project"
                         :keys "p"
                         :before-finalize (lambda () (+org/insert-creation))
                         :template "* %{todo} %?"
                         :file +org/project-org-file-path
                         :contexts ((:function (lambda () (projectile-project-p (buffer-file-name (current-buffer))))))
                         :children (("Task" :keys "p" :todo "TODO" :headline "Tasks")
                                    ("Idea" :keys "i" :todo "IDEA" :headline "Tasks")
                                    ("Note" :keys "n" :template "* %?" :headline "Notes")
                                    ("Snippet" :keys "s" :headline "Notes" :template +core/capture-snippet :contexts (:when (region-active-p)))))
                        ("Feedback"
                         :keys "f"
                         :file +org/notes-file
                         :before-finalize (lambda () (+org/insert-creation))
                         :headline "Feedback"
                         :template "* %?")
                        ("Notes"
                         :keys "n"
                         :before-finalize (lambda () (+org/insert-creation))
                         :file +org/notes-file
                         :headline "Note"
                         :template "* %?"))))))
