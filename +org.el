;; -*- lexical-binding: t; -*-

(defvar +org/org-directory "~/Documents/Org")

(setq org-directory +org/org-directory)

(after! org
  (setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-log-into-drawer t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-treat-insert-todo-heading-as-state-change t

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

      org-id-track-globally t
      org-id-locations-file (concat +org/org-directory "/.orgids")

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
                          (sequence "|" "CANCELED(c@/!)" "EXPIRED(!)"))
      org-todo-keyword-faces '(("IDEA" . +org-todo-active)
                               ("TODO" . +org-todo-active)
                               ("IN-PROGRESS" . +org-todo-active)
                               ("BLOCKED" . +org-todo-onhold)
                               ("DONE" . +org-todo-onhold)
                               ("CANCELED" . +org-todo-onhold)
                               ("EXPIRED" . +org-todo-onhold)))

  ;; Assure that the id file exists, it will crash if the file does not exist
  (unless (file-exists-p org-id-locations-file)
    (with-temp-buffer (write-file org-id-locations-file)))

  (after! recentf
    (add-to-list 'recentf-exclude org-id-locations-file)))

;; (set-popup-rule! "^\\*org" :side 'right :size 80 :select nil :modeline t)


;; Define my different files
(defvar +org/todo-file (concat (file-name-as-directory +org/org-directory) "todo.org"))
(defvar +org/notes-file (concat (file-name-as-directory +org/org-directory) "notes.org"))
(defvar +org/archive-file (concat (file-name-as-directory +org/org-directory) "archive.org"))
(defvar +org/inbox-file (concat (file-name-as-directory +org/org-directory) "inbox.org"))
(defvar +org/calendar-file (concat (file-name-as-directory +org/org-directory) "calendar.org"))

(setq org-archive-location (format "%s::%s" +org/archive-file "* From %s" ))

;; Add agenda files
(after! org
  (when (file-exists-p +org/calendar-file)
    (add-to-list 'org-agenda-files +org/calendar-file))

  (when (file-exists-p +org/todo-file)
    (add-to-list 'org-agenda-files +org/todo-file)))

;; org journal
(setq org-journal-file-format "%Y-%m-%d"
      org-journal-date-format "%A, %Y-%m-%d"
      org-journal-file-header #'+org/org-journal-file-header-func)

;; (map! :localleader :map org-journal-mode-map
;;       :desc "Previous Entry" :ng "p" #'org-journal-previous-entry
;;       :desc "Next Entry" :ng "n" #'org-journal-next-entry)

;; (map! (:leader
;;         (:prefix "a"
;;           (:prefix ("o" . "org")
;;             :desc "Agenda" :ng "a" #'org-agenda
;;             :desc "Capture" :ng "c" #'org-capture
;;             :desc "Tags View" :ng "m" #'org-tags-view
;;             :desc "Search View" :ng "s" #'org-search-view
;;             :desc "Todo List" :ng "t" #'org-todo-list
;;             (:prefix ("j" . "journal")
;;               :desc "New Entry" :ng "j" #'org-journal-new-entry
;;               :desc "Open Journal" :ng "o" #'+org/open-todays-journal)))
;;         (:prefix "s"
;;           :desc "Search Org Directory" :ng "o" #'+default/org-notes-search)
;;         (:prefix "f"
;;           (:prefix ("o" . "org")
;;             :desc "Browse Org files" :ng "b" #'+default/browse-notes
;;             :desc "Todo File" :ng "t" (cmd! () (find-file +org/todo-file))
;;             :desc "Notes File" :ng "n" (cmd! () (find-file +org/notes-file))
;;             :desc "Calendar File" :ng "c" (cmd! () (find-file +org/calendar-file))
;;             :desc "Inbox File" :ng "i" (cmd! () (find-file +org/inbox-file))
;;             :desc "Archive File" :ng "a" (cmd! () (find-file +org/archive-file))
;;             :desc "Today's Journal" :ng "j" #'+org/open-todays-journal))
;;         (:prefix "p"
;;           :desc "Open Org File" :ng "o" (cmd! () (when (projectile-project-p)
;;                                                      (find-file (+org/project-org-file-path)))))))

;; (after! org
;;   (map! (:localleader
;;          :map org-mode-map
;;          ;; Assure that any doom bindings are first cleared
;;          :ng "'" nil
;;          :ng "+" nil
;;          :ng "#" nil
;;          :ng "," nil
;;          :ng "." nil
;;          :ng "/" nil
;;          :ng "A" nil
;;          :ng "I" nil
;;          :ng "T" nil
;;          :ng "a" nil
;;          :ng "b" nil
;;          :ng "c" nil
;;          :ng "d" nil
;;          :ng "e" nil
;;          :ng "f" nil
;;          :ng "g" nil
;;          :ng "h" nil
;;          :ng "i" nil
;;          :ng "l" nil
;;          :ng "m" nil
;;          :ng "n" nil
;;          :ng "o" nil
;;          :ng "p" nil
;;          :ng "q" nil
;;          :ng "r" nil
;;          :ng "s" nil
;;          :ng "t" nil
;;          :ng "x" nil

;;          :desc "C-c C-*" :ng "*" #'org-ctrl-c-star
;;          :desc "C-c C--" :ng "-" #'org-ctrl-c-minus
;;          :desc "C-c C-c" :ng "," #'org-ctrl-c-ctrl-c
;;          :desc "Edit Special" :ng "'" #'org-edit-special

;;          (:prefix ("B" . "brain")
;;           :when (featurep! :lang org +brain)
;;           :desc "Refile" :ng "R" #'org-brain-refile
;;           :desc "Delete Entry" :ng "x" #'org-brain-delete-entry
;;           :desc "Visualize" :ng "v" #'org-brain-visualize
;;           (:prefix ("a" . "add")
;;            :desc "Child" :ng "c" #'org-brain-add-child
;;            :desc "Headline Child" :ng "h" #'org-brain-add-child-headline
;;            :desc "Friendship" :ng "f" #'org-brain-add-friendship
;;            :desc "Parent" :ng "p" #'org-brain-add-parent)
;;           (:prefix ("g" . "goto")
;;            :desc "Goto" :ng "g" #'org-brain-goto
;;            :desc "Child" :ng "c" #'org-brain-goto-child
;;            :desc "Parent" :ng "p" #'org-brain-goto-parent
;;            :desc "Friend<" :ng "f" #'org-brain-goto-friend))

;;          (:prefix ("a" . "agenda")
;;           :desc "Agenda" :ng "a" #'org-agenda
;;           :desc "Expire And Archive Tasks" :ng "e" #'+org/expire-and-archive-tasks-in-buffer
;;           :desc "Archive Completed" :ng "E" #'+org/org-archive-completed-in-buffer
;;           :desc "Close Expired Tasks" :ng "c" #'+org/close-expired-in-buffer)
;;          (:prefix ("i" . "insert")
;;           :desc "Drawer" :ng "d" #'org-insert-drawer
;;           :desc "Item" :ng "i" #'org-insert-item
;;           :desc "Note" :ng "n" #'org-add-note
;;           :desc "Property" :ng "p" #'org-set-property
;;           :desc "Tags" :ng "t" #'org-set-tags-command
;;           :desc "Attach" :ng "a" #'org-attach
;;           :desc "Id Link" :ng "l" #'+org/org-insert-custom-id-link
;;           :desc "Link" :ng "L" #'org-insert-link
;;           :desc "New Footnote" :ng "f" #'org-footnote-new
;;           (:when (featurep! :lang org +dragndrop)
;;            (:prefix ("D" . "download")
;;             :desc "Yank" :ng "y" #'org-download-yank
;;             :desc "Screenshot" :ng "s" #'org-download-screenshot))
;;           (:when (featurep! :completion ivy)
;;            :desc "Counsel Tag" :ng "T" #'counsel-org-tag))
;;          (:prefix ("d" . "dates")
;;           :desc "Toggle Timestamp Type" :ng "c" #'org-toggle-timestamp-type
;;           :desc "Set Schedule" :ng "s" #'org-schedule
;;           :desc "Set Deadline" :ng "d" #'org-deadline
;;           :desc "Set Expiry" :ng "e" #'org-expiry-insert-expiry
;;           :desc "Set Expire on time (dwim)" :ng "E" #'+org/expire-on-time-dwim
;;           :desc "Set Time Stamp" :ng "t" #'org-time-stamp
;;           :desc "Set Time Stamp (inactive)" :ng "T" #'org-time-stamp-inactive)
;;          (:prefix ("h" . "headings")
;;           :desc "Insert Heading After Current" :ng "i" #'org-insert-heading-after-current
;;           :desc "Insert Heading" :ng "I" #'org-insert-heading
;;           :desc "Insert Subheading" :ng "s" #'org-insert-subheading)
;;          (:prefix ("e" . "export")
;;           :desc "Export Dispatch" :ng "e" #'org-export-dispatch)
;;          (:prefix ("b" . "babel")
;;           :desc "Previous Source Block" :ng "p" #'org-babel-previous-src-block
;;           :desc "Next Source Block" :ng "n" #'org-babel-next-src-block
;;           :desc "Execute Maybe" :ng "e" #'org-babel-execute-maybe
;;           :desc "Open Block result" :ng "o" #'org-babel-open-src-block-result
;;           :desc "Expand Source Block" :ng "v" #'org-babel-expand-src-block
;;           :desc "Goto Source Block Head" :ng "u" #'org-babel-goto-src-block-head
;;           :desc "Goto Named Source Block" :ng "g" #'org-babel-goto-named-src-block
;;           :desc "Goto Named Result" :ng "r" #'org-babel-goto-named-result
;;           :desc "Execute Buffer" :ng "b" #'org-babel-execute-buffer
;;           :desc "Execute Subtree" :ng "s" #'org-babel-execute-subtree
;;           :desc "Demarcate Block" :ng "d" #'org-babel-demarcate-block
;;           :desc "Tangle" :ng "t" #'org-babel-tangle
;;           :desc "Tangle File" :ng "f" #'org-babel-tangle-file
;;           :desc "Check Source Block" :ng "c" #'org-babel-check-src-block
;;           :desc "Insert Header Arg" :ng "j" #'org-babel-insert-header-arg
;;           :desc "Load In Session" :ng "l" #'org-babel-load-in-session
;;           :desc "Lob Ingest" :ng "i" #'org-babel-lob-ingest
;;           :desc "View Source Block Info" :ng "I" #'org-babel-view-src-block-info
;;           :desc "Switch To Session" :ng "z" #'org-babel-switch-to-session
;;           :desc "Switch To Session With Code" :ng "Z" #'org-babel-switch-to-session-with-code
;;           :desc "SHA1 Hash" :ng "a" #'org-babel-sha1-hash
;;           :desc "Do Key Sequence In Edit Buffer" :ng "x" #'org-babel-do-key-sequence-in-edit-buffer)
;;          (:prefix ("x" . "text")
;;           :desc "Open Link" :ng "o" #'org-open-at-point
;;           :desc "Bold" :ng "b" (cmd! () (org-emphasize ?*))
;;           :desc "Code" :ng "c" (cmd! () (org-emphasize ?~))
;;           :desc "Italic" :ng "i" (cmd! () (org-emphasize ?/))
;;           :desc "Clear" :ng "r" (cmd! () (org-emphasize ? ))
;;           :desc "Strike Through" :ng "s" (cmd! () (org-emphasize ?+))
;;           :desc "Underline" :ng "u" (cmd! () (org-emphasize ?_))
;;           :desc "Verbose" :ng "v" (cmd! () (org-emphasize ?=)))
;;          (:prefix ("s" . "sub-tree")
;;           :desc "Toggle Archive Tag" :ng "a" #'org-toggle-archive-tag
;;           :desc "Archive Sub-tree" :ng "A" #'org-archive-subtree
;;           :desc "Tree To Indirect Buffer" :ng "b" #'org-tree-to-indirect-buffer
;;           :desc "Cut Sub-tree" :ng "k" #'org-cut-subtree
;;           :desc "Narrow To Sub-tree" :ng "n" #'org-narrow-to-subtree
;;           :desc "Widen" :ng "N" #'widen
;;           :desc "Refile" :ng "r" #'org-refile
;;           :desc "Sparse Tree" :ng "s" #'org-sparse-tree
;;           :desc "Sort" :ng "S" #'org-sort)
;;          (:prefix ("l" . "links")
;;           :desc "Store Link" :ng "s" #'org-store-link
;;           :desc "Insert Link" :ng "l" #'org-insert-link
;;           :desc "Id Link" :ng "i" #'+org/org-insert-custom-id-link
;;           :desc "Remove Link" :ng "k" #'+org/remove-link
;;           :desc "Insert All Links" :ng "L" #'org-insert-all-links
;;           :desc "Clip Link" :ng "c" #'org-cliplink
;;           :desc "Next Link" :ng "n" #'org-next-link
;;           :desc "Previous Link" :ng "p" #'org-previous-link)
;;          (:prefix ("t" . "tables")
;;           :desc "Align" :ng "a" #'org-table-align
;;           :desc "Blank Field" :ng "b" #'org-table-blank-field
;;           :desc "Convert" :ng "c" #'org-table-convert
;;           :desc "Eval Formula" :ng "e" #'org-table-eval-formula
;;           :desc "Export" :ng "E" #'org-table-export
;;           :desc "Field Info" :ng "f" #'org-table-field-info
;;           :desc "Previous Field" :ng "h" #'org-table-previous-field
;;           :desc "Move Column Left" :ng "H" #'org-table-move-column-left
;;           :desc "Import" :ng "I" #'org-table-import
;;           :desc "Next Row" :ng "j" #'org-table-next-row
;;           :desc "Move Row Down" :ng "J" #'org-table-move-row-down
;;           :desc "Move Row Up" :ng "K" #'org-table-move-row-up
;;           :desc "Next Field" :ng "l" #'org-table-next-field
;;           :desc "Move Column Right" :ng "L" #'org-table-move-column-right
;;           :desc "Create" :ng "n" #'org-table-create
;;           :desc "Create with table.el" :ng "N" #'org-table-create-with-table.el
;;           :desc "Re-calculate" :ng "r" #'org-table-recalculate
;;           :desc "Sort Lines" :ng "s" #'org-table-sort-lines
;;           :desc "Wrap Region" :ng "w" #'org-table-wrap-region
;;           (:prefix ("i" . "insert")
;;            :desc "Insert Column" :ng "c" #'org-table-insert-column
;;            :desc "Insert Horizontal Line" :ng "h" #'org-table-insert-hline
;;            :desc "Horizontal Line And Move" :ng "H" #'org-table-hline-and-move)
;;           (:prefix ("t" . "toggle")
;;            :desc "Formula Debugger" :ng "f" #'org-table-toggle-formula-debugger
;;            :desc "Coordinate Overlays" :ng "o" #'org-table-toggle-coordinate-overlays)
;;           (:prefix ("d" . "delete")
;;            :desc "Delete Column" :ng "c" #'org-table-delete-column
;;            :desc "Delete Row" :ng "r" #'org-table-kill-row))
;;          (:prefix ("T" . "toggle")
;;           :desc "Checkbox" :ng "c" #'org-toggle-checkbox
;;           :desc "Pretty Entities" :ng "e" #'org-toggle-pretty-entities
;;           :desc "Inline Images" :ng "i" #'org-toggle-inline-images
;;           :desc "Link Display" :ng "l" #'org-toggle-link-display
;;           :desc "Show Todo Tree" :ng "t" #'org-show-todo-tree
;;           :desc "Org Todo" :ng "T" #'org-todo
;;           :desc "Preview Latex Fragment" :ng "x" #'org-latex-preview))))

;;;###package
(use-package! demo-it
  :defer t
  :after org)

;; ;;;###package
;; ;(use-package! idle-org-agenda
;; ;  :after-call pre-command-hook
;; ;  :custom (idle-org-agenda-interval 600)
;; ;  :config (idle-org-agenda-mode))

;;;###package
(use-package! org-super-agenda
  :defer t
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
(use-package! org-tempo ;; for <s templates etc.
  :defer t
  :after org)

(after! org

  (use-package! org-id
    :defer t
    :after org)

  (add-hook 'org-capture-prepare-finalize-hook #'+org/insert-id)
  (org-link-set-parameters "id" :store #'org-id-store-link) ;; Make sure that we can create id links

;;;###package
  (use-package! org-make-toc
    :defer t
    :hook (org-mode . org-make-toc-mode))

;;;###package
  (use-package! org-expiry
    :defer t
    :after org
    :commands org-expiry-insert-expiry
    :bind (:map org-mode-map
            ("C-c C-e" . #'org-expiry-insert-expiry))
    :config (setq org-expiry-inactive-timestamps t)
    (add-hook 'org-capture-before-finalize-hook #'+org/insert-creation)
    (add-hook 'org-insert-heading-hook #'+org/insert-creation))

;;;###package
(use-package! doct
  :defer t
  :after org-capture
  :bind (("C-c c" . #'org-capture))
  :init (setq org-capture-templates '())
  :config
  (add-to-list 'org-agenda-files (+org/projects-directory))
  (setq org-capture-templates
        (append org-capture-templates
                (doct '(("Tasks"
                         :keys "t"
                         :file +org/todo-file
                         :template "* %{todo} %?"
                         :children (("Task" :keys "t" :todo "TODO")
                                    ("Idea" :keys "i" :todo "IDEA")
                                    ("Reminder" :keys "r" :template "* TODO %?\nSCHEDULED: %^t")))
                        ("Project"
                         :keys "p"
                         :template "* %{todo} %?"
                         :file +org/project-org-file-path
                         :contexts ((:function (lambda () (projectile-project-p (buffer-file-name (current-buffer))))))
                         :children (("Task" :keys "p" :todo "TODO" :headline "Tasks")
                                    ("Idea" :keys "i" :todo "IDEA" :headline "Tasks")
                                    ("Note" :keys "n" :template "* %?" :headline "Notes")
                                    ("Snippet" :keys "s" :headline "Notes" :template +core/capture-snippet)))
                        ("Journal Entry"
                         :keys "j"
                         :type plain
                         :template "** %(format-time-string org-journal-time-format) %?"
                         :function +org/org-journal-find-location)
                        ("Feedback"
                         :keys "f"
                         :file +org/notes-file
                         :headline "Feedback"
                         :template "* %?")
                        ("Notes"
                         :keys "n"
                         :file +org/notes-file
                         :headline "Notes"
                         :template "* %?")))))))
