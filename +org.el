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
;;       :desc "Previous Entry" :nvg "p" #'org-journal-previous-entry
;;       :desc "Next Entry" :nvg "n" #'org-journal-next-entry)

;; (map! (:leader
;;         (:prefix "a"
;;           (:prefix ("o" . "org")
;;             :desc "Agenda" :nvg "a" #'org-agenda
;;             :desc "Capture" :nvg "c" #'org-capture
;;             :desc "Tags View" :nvg "m" #'org-tags-view
;;             :desc "Search View" :nvg "s" #'org-search-view
;;             :desc "Todo List" :nvg "t" #'org-todo-list
;;             (:prefix ("j" . "journal")
;;               :desc "New Entry" :nvg "j" #'org-journal-new-entry
;;               :desc "Open Journal" :nvg "o" #'+org/open-todays-journal)))
;;         (:prefix "s"
;;           :desc "Search Org Directory" :nvg "o" #'+default/org-notes-search)
;;         (:prefix "f"
;;           (:prefix ("o" . "org")
;;             :desc "Browse Org files" :nvg "b" #'+default/browse-notes
;;             :desc "Todo File" :nvg "t" (cmd! () (find-file +org/todo-file))
;;             :desc "Notes File" :nvg "n" (cmd! () (find-file +org/notes-file))
;;             :desc "Calendar File" :nvg "c" (cmd! () (find-file +org/calendar-file))
;;             :desc "Inbox File" :nvg "i" (cmd! () (find-file +org/inbox-file))
;;             :desc "Archive File" :nvg "a" (cmd! () (find-file +org/archive-file))
;;             :desc "Today's Journal" :nvg "j" #'+org/open-todays-journal))
;;         (:prefix "p"
;;           :desc "Open Org File" :nvg "o" (cmd! () (when (projectile-project-p)
;;                                                      (find-file (+org/project-org-file-path)))))))

;; (after! org
;;   (map! (:localleader
;;          :map org-mode-map
;;          ;; Assure that any doom bindings are first cleared
;;          :nvg "'" nil
;;          :nvg "+" nil
;;          :nvg "#" nil
;;          :nvg "," nil
;;          :nvg "." nil
;;          :nvg "/" nil
;;          :nvg "A" nil
;;          :nvg "I" nil
;;          :nvg "T" nil
;;          :nvg "a" nil
;;          :nvg "b" nil
;;          :nvg "c" nil
;;          :nvg "d" nil
;;          :nvg "e" nil
;;          :nvg "f" nil
;;          :nvg "g" nil
;;          :nvg "h" nil
;;          :nvg "i" nil
;;          :nvg "l" nil
;;          :nvg "m" nil
;;          :nvg "n" nil
;;          :nvg "o" nil
;;          :nvg "p" nil
;;          :nvg "q" nil
;;          :nvg "r" nil
;;          :nvg "s" nil
;;          :nvg "t" nil
;;          :nvg "x" nil

;;          :desc "C-c C-*" :nvg "*" #'org-ctrl-c-star
;;          :desc "C-c C--" :nvg "-" #'org-ctrl-c-minus
;;          :desc "C-c C-c" :nvg "," #'org-ctrl-c-ctrl-c
;;          :desc "Edit Special" :nvg "'" #'org-edit-special

;;          (:prefix ("B" . "brain")
;;           :when (featurep! :lang org +brain)
;;           :desc "Refile" :nvg "R" #'org-brain-refile
;;           :desc "Delete Entry" :nvg "x" #'org-brain-delete-entry
;;           :desc "Visualize" :nvg "v" #'org-brain-visualize
;;           (:prefix ("a" . "add")
;;            :desc "Child" :nvg "c" #'org-brain-add-child
;;            :desc "Headline Child" :nvg "h" #'org-brain-add-child-headline
;;            :desc "Friendship" :nvg "f" #'org-brain-add-friendship
;;            :desc "Parent" :nvg "p" #'org-brain-add-parent)
;;           (:prefix ("g" . "goto")
;;            :desc "Goto" :nvg "g" #'org-brain-goto
;;            :desc "Child" :nvg "c" #'org-brain-goto-child
;;            :desc "Parent" :nvg "p" #'org-brain-goto-parent
;;            :desc "Friend<" :nvg "f" #'org-brain-goto-friend))

;;          (:prefix ("a" . "agenda")
;;           :desc "Agenda" :nvg "a" #'org-agenda
;;           :desc "Expire And Archive Tasks" :nvg "e" #'+org/expire-and-archive-tasks-in-buffer
;;           :desc "Archive Completed" :nvg "E" #'+org/org-archive-completed-in-buffer
;;           :desc "Close Expired Tasks" :nvg "c" #'+org/close-expired-in-buffer)
;;          (:prefix ("i" . "insert")
;;           :desc "Drawer" :nvg "d" #'org-insert-drawer
;;           :desc "Item" :nvg "i" #'org-insert-item
;;           :desc "Note" :nvg "n" #'org-add-note
;;           :desc "Property" :nvg "p" #'org-set-property
;;           :desc "Tags" :nvg "t" #'org-set-tags-command
;;           :desc "Attach" :nvg "a" #'org-attach
;;           :desc "Id Link" :nvg "l" #'+org/org-insert-custom-id-link
;;           :desc "Link" :nvg "L" #'org-insert-link
;;           :desc "New Footnote" :nvg "f" #'org-footnote-new
;;           (:when (featurep! :lang org +dragndrop)
;;            (:prefix ("D" . "download")
;;             :desc "Yank" :nvg "y" #'org-download-yank
;;             :desc "Screenshot" :nvg "s" #'org-download-screenshot))
;;           (:when (featurep! :completion ivy)
;;            :desc "Counsel Tag" :nvg "T" #'counsel-org-tag))
;;          (:prefix ("d" . "dates")
;;           :desc "Toggle Timestamp Type" :nvg "c" #'org-toggle-timestamp-type
;;           :desc "Set Schedule" :nvg "s" #'org-schedule
;;           :desc "Set Deadline" :nvg "d" #'org-deadline
;;           :desc "Set Expiry" :nvg "e" #'org-expiry-insert-expiry
;;           :desc "Set Expire on time (dwim)" :nvg "E" #'+org/expire-on-time-dwim
;;           :desc "Set Time Stamp" :nvg "t" #'org-time-stamp
;;           :desc "Set Time Stamp (inactive)" :nvg "T" #'org-time-stamp-inactive)
;;          (:prefix ("h" . "headings")
;;           :desc "Insert Heading After Current" :nvg "i" #'org-insert-heading-after-current
;;           :desc "Insert Heading" :nvg "I" #'org-insert-heading
;;           :desc "Insert Subheading" :nvg "s" #'org-insert-subheading)
;;          (:prefix ("e" . "export")
;;           :desc "Export Dispatch" :nvg "e" #'org-export-dispatch)
;;          (:prefix ("b" . "babel")
;;           :desc "Previous Source Block" :nvg "p" #'org-babel-previous-src-block
;;           :desc "Next Source Block" :nvg "n" #'org-babel-next-src-block
;;           :desc "Execute Maybe" :nvg "e" #'org-babel-execute-maybe
;;           :desc "Open Block result" :nvg "o" #'org-babel-open-src-block-result
;;           :desc "Expand Source Block" :nvg "v" #'org-babel-expand-src-block
;;           :desc "Goto Source Block Head" :nvg "u" #'org-babel-goto-src-block-head
;;           :desc "Goto Named Source Block" :nvg "g" #'org-babel-goto-named-src-block
;;           :desc "Goto Named Result" :nvg "r" #'org-babel-goto-named-result
;;           :desc "Execute Buffer" :nvg "b" #'org-babel-execute-buffer
;;           :desc "Execute Subtree" :nvg "s" #'org-babel-execute-subtree
;;           :desc "Demarcate Block" :nvg "d" #'org-babel-demarcate-block
;;           :desc "Tangle" :nvg "t" #'org-babel-tangle
;;           :desc "Tangle File" :nvg "f" #'org-babel-tangle-file
;;           :desc "Check Source Block" :nvg "c" #'org-babel-check-src-block
;;           :desc "Insert Header Arg" :nvg "j" #'org-babel-insert-header-arg
;;           :desc "Load In Session" :nvg "l" #'org-babel-load-in-session
;;           :desc "Lob Ingest" :nvg "i" #'org-babel-lob-ingest
;;           :desc "View Source Block Info" :nvg "I" #'org-babel-view-src-block-info
;;           :desc "Switch To Session" :nvg "z" #'org-babel-switch-to-session
;;           :desc "Switch To Session With Code" :nvg "Z" #'org-babel-switch-to-session-with-code
;;           :desc "SHA1 Hash" :nvg "a" #'org-babel-sha1-hash
;;           :desc "Do Key Sequence In Edit Buffer" :nvg "x" #'org-babel-do-key-sequence-in-edit-buffer)
;;          (:prefix ("x" . "text")
;;           :desc "Open Link" :nvg "o" #'org-open-at-point
;;           :desc "Bold" :nvg "b" (cmd! () (org-emphasize ?*))
;;           :desc "Code" :nvg "c" (cmd! () (org-emphasize ?~))
;;           :desc "Italic" :nvg "i" (cmd! () (org-emphasize ?/))
;;           :desc "Clear" :nvg "r" (cmd! () (org-emphasize ? ))
;;           :desc "Strike Through" :nvg "s" (cmd! () (org-emphasize ?+))
;;           :desc "Underline" :nvg "u" (cmd! () (org-emphasize ?_))
;;           :desc "Verbose" :nvg "v" (cmd! () (org-emphasize ?=)))
;;          (:prefix ("s" . "sub-tree")
;;           :desc "Toggle Archive Tag" :nvg "a" #'org-toggle-archive-tag
;;           :desc "Archive Sub-tree" :nvg "A" #'org-archive-subtree
;;           :desc "Tree To Indirect Buffer" :nvg "b" #'org-tree-to-indirect-buffer
;;           :desc "Cut Sub-tree" :nvg "k" #'org-cut-subtree
;;           :desc "Narrow To Sub-tree" :nvg "n" #'org-narrow-to-subtree
;;           :desc "Widen" :nvg "N" #'widen
;;           :desc "Refile" :nvg "r" #'org-refile
;;           :desc "Sparse Tree" :nvg "s" #'org-sparse-tree
;;           :desc "Sort" :nvg "S" #'org-sort)
;;          (:prefix ("l" . "links")
;;           :desc "Store Link" :nvg "s" #'org-store-link
;;           :desc "Insert Link" :nvg "l" #'org-insert-link
;;           :desc "Id Link" :nvg "i" #'+org/org-insert-custom-id-link
;;           :desc "Remove Link" :nvg "k" #'+org/remove-link
;;           :desc "Insert All Links" :nvg "L" #'org-insert-all-links
;;           :desc "Clip Link" :nvg "c" #'org-cliplink
;;           :desc "Next Link" :nvg "n" #'org-next-link
;;           :desc "Previous Link" :nvg "p" #'org-previous-link)
;;          (:prefix ("t" . "tables")
;;           :desc "Align" :nvg "a" #'org-table-align
;;           :desc "Blank Field" :nvg "b" #'org-table-blank-field
;;           :desc "Convert" :nvg "c" #'org-table-convert
;;           :desc "Eval Formula" :nvg "e" #'org-table-eval-formula
;;           :desc "Export" :nvg "E" #'org-table-export
;;           :desc "Field Info" :nvg "f" #'org-table-field-info
;;           :desc "Previous Field" :nvg "h" #'org-table-previous-field
;;           :desc "Move Column Left" :nvg "H" #'org-table-move-column-left
;;           :desc "Import" :nvg "I" #'org-table-import
;;           :desc "Next Row" :nvg "j" #'org-table-next-row
;;           :desc "Move Row Down" :nvg "J" #'org-table-move-row-down
;;           :desc "Move Row Up" :nvg "K" #'org-table-move-row-up
;;           :desc "Next Field" :nvg "l" #'org-table-next-field
;;           :desc "Move Column Right" :nvg "L" #'org-table-move-column-right
;;           :desc "Create" :nvg "n" #'org-table-create
;;           :desc "Create with table.el" :nvg "N" #'org-table-create-with-table.el
;;           :desc "Re-calculate" :nvg "r" #'org-table-recalculate
;;           :desc "Sort Lines" :nvg "s" #'org-table-sort-lines
;;           :desc "Wrap Region" :nvg "w" #'org-table-wrap-region
;;           (:prefix ("i" . "insert")
;;            :desc "Insert Column" :nvg "c" #'org-table-insert-column
;;            :desc "Insert Horizontal Line" :nvg "h" #'org-table-insert-hline
;;            :desc "Horizontal Line And Move" :nvg "H" #'org-table-hline-and-move)
;;           (:prefix ("t" . "toggle")
;;            :desc "Formula Debugger" :nvg "f" #'org-table-toggle-formula-debugger
;;            :desc "Coordinate Overlays" :nvg "o" #'org-table-toggle-coordinate-overlays)
;;           (:prefix ("d" . "delete")
;;            :desc "Delete Column" :nvg "c" #'org-table-delete-column
;;            :desc "Delete Row" :nvg "r" #'org-table-kill-row))
;;          (:prefix ("T" . "toggle")
;;           :desc "Checkbox" :nvg "c" #'org-toggle-checkbox
;;           :desc "Pretty Entities" :nvg "e" #'org-toggle-pretty-entities
;;           :desc "Inline Images" :nvg "i" #'org-toggle-inline-images
;;           :desc "Link Display" :nvg "l" #'org-toggle-link-display
;;           :desc "Show Todo Tree" :nvg "t" #'org-show-todo-tree
;;           :desc "Org Todo" :nvg "T" #'org-todo
;;           :desc "Preview Latex Fragment" :nvg "x" #'org-latex-preview))))

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
