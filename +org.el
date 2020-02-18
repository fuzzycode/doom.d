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

;; TODO(Björn Larsson): Expand this list with more modes that should be supported
(setq +org/major-mode-to-org-src
      '((c++-mode . "C++")
        (python-mode . "python")
        (emacs-lisp-mode . "emacs-lisp")
        (shell-mode . "sh")
        (lua-mode . "lua")
        (json-mode . "json")
        (yaml-mode . "yml")
        (cmake-mode . "cmake")))

(defun +core/capture-snippet ()
  "Formats a capture snippet for capturing code."
  (let ((line-number (line-number-at-pos (region-beginning)))
         (func-name (which-function))
         (org-src-mode (cdr (assoc major-mode +org/major-mode-to-org-src))))
    (format "* %%?\nSource: [[file:%%F::%d][%%f (%s)]]\n#+begin_src %s\n%%i\n#+end_src" line-number func-name (or org-src-mode ""))))


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

(map! :after org
      (:localleader
        :map org-mode-map
        ;; Assure that any doom bindings are first cleared
        :g "'" nil
        :g "," nil
        :g "." nil
        :g "/" nil
        :g "A" nil
        :g "I" nil
        :g "T" nil
        :g "a" nil
        :g "b" nil
        :g "c" nil
        :g "d" nil
        :g "e" nil
        :g "f" nil
        :g "g" nil
        :g "h" nil
        :g "i" nil
        :g "n" nil
        :g "o" nil
        :g "p" nil
        :g "q" nil
        :g "s" nil
        :g "t" nil
        :g "x" nil

        :desc "C-c C-*" :g "*" #'org-ctrl-c-star
        :desc "C-c C--" :g "-" #'org-ctrl-c-minus
        :desc "C-c C-c" :g "," #'org-ctrl-c-ctrl-c
        :desc "Edit Special" :g "'" #'org-edit-special

        (:prefix ("B" . "brain")
          :when (featurep! :lang org +brain)
          :desc "Refile" :g "R" #'org-brain-refile
          :desc "Delete Entry" :g "x" #'org-brain-delete-entry
          :desc "Visualize" :g "v" #'org-brain-visualize
          (:prefix ("a" . "add")
            :desc "Child" :g "c" #'org-brain-add-child
            :desc "Headline Child" :g "h" #'org-brain-add-child-headline
            :desc "Friendship" :g "f" #'org-brain-add-friendship
            :desc "Parent" :g "p" #'org-brain-add-parent)
          (:prefix ("g" . "goto")
            :desc "Goto" :g "g" #'org-brain-goto
            :desc "Child" :g "c" #'org-brain-goto-child
            :desc "Parent" :g "p" #'org-brain-goto-parent
            :desc "Friend<" :g "f" #'org-brain-goto-friend))

        (:prefix ("a" . "agenda")
          :desc "Agenda" :g "a" #'org-agenda
          :desc "Expire And Archive Tasks" :g "e" #'+org/expire-and-archive-tasks
          :desc "Archive Completed" :g "E" #'+org/org-archive-completed-in-buffer)
        (:prefix ("i" . "insert")
          :desc "Drawer" :g "d" #'org-insert-drawer
          :desc "Item" :g "i" #'org-insert-item
          :desc "Note" :g "n" #'org-add-note
          :desc "Property" :g "p" #'org-set-property
          :desc "Tags" :g "t" #'org-set-tags
          :desc "Attach" :g "a" #'org-attach
          :desc "Link" :g "l" #'org-insert-link
          :desc "New Footnote" :g "f" #'org-footnote-new
          (:when (featurep! :lang org +dragndrop)
            (:prefix ("D" . "download")
              :desc "Yank" :g "y" #'org-download-yank
              :desc "Screenshot" :g "s" #'org-download-screenshot)))
        (:prefix ("d" . "deadline")
          :desc "Schedule" :g "s" #'org-schedule
          :desc "Deadline" :g "d" #'org-deadline
          :desc "Expiry" :g "e" #'org-expiry-insert-expiry
          :desc "Time Stamp" :g "t" #'org-time-stamp
          :desc "Time Stamp (inactive)" :g "T" #'org-time-stamp-inactive)
        (:prefix ("h" . "headings")
          :desc "Insert Heading After Current" :g "i" #'org-insert-heading-after-current
          :desc "Insert Heading" :g "I" #'org-insert-heading
          :desc "Insert Subheading" :g "s" #'org-insert-subheading)
        (:prefix ("e" . "export")
          :desc "Export Dispatch" :g "e" #'org-export-dispatch)
        (:prefix ("b" . "babel")
          :desc "Previous Source Block" :g "p" #'org-babel-previous-src-block
          :desc "Next Source Block" :g "n" #'org-babel-next-src-block
          :desc "Execute Maybe" :g "e" #'org-babel-execute-maybe
          :desc "Open Block result" :g "o" #'org-babel-open-src-block-result
          :desc "Expand Source Block" :g "v" #'org-babel-expand-src-block
          :desc "Goto Source Block Head" :g "u" #'org-babel-goto-src-block-head
          :desc "Goto Named Source Block" :g "g" #'org-babel-goto-named-src-block
          :desc "Goto Named Result" :g "r" #'org-babel-goto-named-result
          :desc "Execute Buffer" :g "b" #'org-babel-execute-buffer
          :desc "Execute Subtree" :g "s" #'org-babel-execute-subtree
          :desc "Demarcate Block" :g "d" #'org-babel-demarcate-block
          :desc "Tangle" :g "t" #'org-babel-tangle
          :desc "Tangle File" :g "f" #'org-babel-tangle-file
          :desc "Check Source Block" :g "c" #'org-babel-check-src-block
          :desc "Insert Header Arg" :g "j" #'org-babel-insert-header-arg
          :desc "Load In Session" :g "l" #'org-babel-load-in-session
          :desc "Lob Ingest" :g "i" #'org-babel-lob-ingest
          :desc "View Source Block Info" :g "I" #'org-babel-view-src-block-info
          :desc "Switch To Session" :g "z" #'org-babel-switch-to-session
          :desc "Switch To Session With Code" :g "Z" #'org-babel-switch-to-session-with-code
          :desc "SHA1 Hash" :g "a" #'org-babel-sha1-hash
          :desc "Do Key Sequence In Edit Buffer" :g "x" #'org-babel-do-key-sequence-in-edit-buffer)
        (:prefix ("x" . "text")
          :desc "Open Link" :g "o" #'org-open-at-point
          :desc "Bold" :g "b" (lambda! () (org-emphasize ?*))
          :desc "Code" :g "c" (lambda! () (org-emphasize ?~))
          :desc "Italic" :g "i" (lambda! () (org-emphasize ?/))
          :desc "Clear" :g "r" (lambda! () (org-emphasize ? ))
          :desc "Strike Through" :g "s" (lambda! () (org-emphasize ?+))
          :desc "Underline" :g "u" (lambda! () (org-emphasize ?_))
          :desc "Verbose" :g "v" (lambda! () (org-emphasize ?=)))
        (:prefix ("s" . "sub-tree")
          :desc "Toggle Archive Tag" :g "a" #'org-toggle-archive-tag
          :desc "Archive Sub-tree" :g "A" #'org-archive-subtree
          :desc "Tree To Indirect Buffer" :g "b" #'org-tree-to-indirect-buffer
          :desc "Cut Sub-tree" :g "k" #'org-cut-subtree
          :desc "Narrow To Sub-tree" :g "n" #'org-narrow-to-subtree
          :desc "Widen" :g "N" #'widen
          :desc "Refile" :g "r" #'org-refile
          :desc "Sparse Tree" :g "s" #'org-sparse-tree
          :desc "Sort" :g "S" #'org-sort)
        (:prefix ("t" . "tables")
          :desc "Align" :g "a" #'org-table-align
          :desc "Blank Field" :g "b" #'org-table-blank-field
          :desc "Convert" :g "c" #'org-table-convert
          :desc "Eval Formula" :g "e" #'org-table-eval-formula
          :desc "Export" :g "E" #'org-table-export
          :desc "Field Info" :g "f" #'org-table-field-info
          :desc "Previous Field" :g "h" #'org-table-previous-field
          :desc "Move Column Left" :g "H" #'org-table-move-column-left
          :desc "Import" :g "I" #'org-table-import
          :desc "Next Row" :g "j" #'org-table-next-row
          :desc "Move Row Down" :g "J" #'org-table-move-row-down
          :desc "Move Row Up" :g "K" #'org-table-move-row-up
          :desc "Next Field" :g "l" #'org-table-next-field
          :desc "Move Column Right" :g "L" #'org-table-move-column-right
          :desc "Create" :g "n" #'org-table-create
          :desc "Create with table.el" :g "N" #'org-table-create-with-table.el
          :desc "Re-calculate" :g "r" #'org-table-recalculate
          :desc "Sort Lines" :g "s" #'org-table-sort-lines
          :desc "Wrap Region" :g "w" #'org-table-wrap-region
          (:prefix ("i" . "insert")
            :desc "Insert Column" :g "c" #'org-table-insert-column
            :desc "Insert Horizontal Line" :g "h" #'org-table-insert-hline
            :desc "Horizontal Line And Move" :g "H" #'org-table-hline-and-move)
          (:prefix ("t" . "toggle")
            :desc "Formula Debugger" :g "f" #'org-table-toggle-formula-debugger
            :desc "Coordinate Overlays" :g "o" #'org-table-toggle-coordinate-overlays)
          (:prefix ("d" . "delete")
            :desc "Delete Column" :g "c" #'org-table-delete-column
            :desc "Delete Row" :g "r" #'org-table-kill-row))
        (:prefix ("T" . "toggle")
          :desc "Checkbox" :g "c" #'org-toggle-checkbox
          :desc "Pretty Entities" :g "e" #'org-toggle-pretty-entities
          :desc "Inline Images" :g "i" #'org-toggle-inline-images
          :desc "Link Display" :g "l" #'org-toggle-link-display
          :desc "Show Todo Tree" :g "t" #'org-show-todo-tree
          :desc "Org Todo" :g "T" #'org-todo
          :desc "Preview Latex Fragment" :g "x" #'org-latex-preview)))

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
                                    ("Snippet" :keys "s" :headline "Notes" :template (lambda () (+core/capture-snippet)))))
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
