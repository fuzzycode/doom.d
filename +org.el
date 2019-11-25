
(setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-log-into-drawer t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-treat-insert-todo-heading-as-state-change t

      org-directory "~/Documents/Org"
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

      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes t
      org-refile-allow-creating-parent-nodes 'confirm
      org-outline-path-complete-in-steps nil
      org-todo-keywords '((sequence "IDEA(i!)" "TODO(t!)" "IN-PROGRESS(p!)"  "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")))


(defvar +org/created-property-string "
  :PROPERTIES:
  :CREATED: %U
  :END:
  :LOGBOOK:
  - State \"TODO\"       from              %U
  :END:")

(defvar +org/todo-file (concat (file-name-as-directory org-directory) "todo.org"))
(defvar +org/notes-file (concat (file-name-as-directory org-directory) "notes.org"))
(defvar +org/archive-file (concat (file-name-as-directory org-directory) "archive.org"))
(defvar +org/inbox-file (concat (file-name-as-directory org-directory) "inbox.org"))
(defvar +org/calendar-file (concat (file-name-as-directory org-directory) "calendar.org"))


(map! (:leader (:prefix ("a" . "applications")
                 (:prefix ("o" . "org")
                   :desc "Agenda" :g "a" #'org-agenda
                   :desc "Capture" :g "c" #'org-capture
                   :desc "Todo List" :g "t" #'org-todo-list))
        (:prefix "s"
          :desc "Search Org Directory" :g "n" #'+default/org-notes-search)
        (:prefix "f"
          (:prefix ("o" . "org")
            :desc "Browse Org files" :g "b" #'+default/browse-notes
            :desc "Todo File" :g "t" (lambda () (interactive) (find-file +org/todo-file))
            :desc "Notes File" :g "n" (lambda ()  (interactive) (find-file +org/notes-file))
            :desc "Calendar File" :g "c" (lambda () (interactive) (find-file +org/calendar-file))
            :desc "Inbox File" :g "i" (lambda () (interactive) (find-file +org/inbox-file))
            :desc "Archive File" :g "a" (lambda () (interactive) (find-file +org/archive-file))))))


;; Archive and refiling
(setq org-archive-location (format "%s::%s" +org/archive-file "* From %s" ))

(setq org-refile-target-verify-function '+org/verify-refile-target)

(setq org-refile-targets (quote ((+org/todo-file :maxlevel . 2)
                                 (+org/notes-file :level . 2))))

;; Doom adds a lot capture stuff that I am not interested in so I clear that out
(setq org-capture-templates '())

;; Capturing
;; (after! org
;;   (add-to-list 'org-capture-templates
;;                `("t" "Todo" entry (file ,+org/todo-file)
;;                  (function +org/make-org-todo-template)))

;;   (add-to-list 'org-capture-templates
;;                `("i" "Idea" entry (file ,+org/todo-file)
;;                  (function +org/make-org-todo-idea)))

;;   (add-to-list 'org-capture-templates
;;                `("I" "In Progress Item" entry (file ,+org/todo-file)
;;                  (function +org/make-org-todo-in-progress)))

;;   (add-to-list 'org-capture-templates
;;                `("n" "Note" entry (file+headline ,+org/notes-file, "Inbox")
;;                  (function +org/make-org-note-template))))

(after! org-projectile
  (push (org-projectile-project-todo-entry
         :capture-template (format "%s%s" "* TODO %?" +org/created-property-string)
         :capture-character "p") org-capture-templates))

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

;; org-agenda

(setq org-agenda-files '())

(when (file-exists-p +org/todo-file)
  (add-to-list 'org-agenda-files +org/todo-file))

(when (file-exists-p +org/calendar-file)
  (add-to-list 'org-agenda-files +org/calendar-file))


(setq org-agenda-time-grid
      '((daily today)
        (800 1000 1200 1400 1600 1800 2000)
        "......"
        "----------------"))

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
