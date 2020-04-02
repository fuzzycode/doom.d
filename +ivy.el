;;; ~/.doom.d/+ivy.el -*- lexical-binding: t; -*-

(map! (:leader
        :desc "M-X" :g "<SPC>" #'counsel-M-x
        :desc "M-X" :g "M-<SPC>" #'counsel-M-x
        :desc "Ripgrep" :g "/" #'counsel-rg
        (:prefix ("a" . "applications")
          :desc "Load Theme" :g "T" #'counsel-load-theme)
        (:prefix ("b" . "buffer/bookmarks")
          :desc "Buffer List" :g "b" #'counsel-switch-buffer
          :desc "Bookmarks" :g "B" #'counsel-bookmark)
        (:prefix ("f" . "files")
          :desc "Find File" :g "f" #'counsel-find-file
          :desc "Recent Files" :g "r" #'counsel-recentf)
        (:prefix ("j" . "jump/join")
          :desc "iMenu" :g "i" #'counsel-imenu)
        (:prefix ("p" . "project")
          :desc "Find Directory" :g "d" #'counsel-projectile-find-dir
          :desc "Find File" :g "f" #'counsel-projectile-find-file
          :desc "Switch Project" :g "p" #'counsel-projectile-switch-project
          :desc "Switch Buffer" :g "b" #'counsel-projectile-switch-to-buffer
          :desc "Search Project From CWD" :g "D" #'+ivy/project-search-from-cwd)
        (:prefix ("r" . "resume")
          :desc "Ivy Resume Last" :g "l" #'ivy-resume)
        (:prefix ("s" . "search")
          :desc "Swiper" :g "s" #'swiper
          :desc "Swiper (input)" :g "S" #'+swiper/swiper-default
          :desc "Locate" :g "l" #'counsel-locate
          :desc "Search Project" :g "p" #'+ivy/project-search
          :desc "Search Project (input)" :g "P" #'+ivy/ivy-search-project-default)))

(after! ivy
  (setq ivy-count-format "(%d/%d) "
        ivy-wrap nil
        ivy-use-virtual-buffers nil
        ivy-fixed-height-minibuffer t
        ivy-display-style 'fancy))

(after! ivy-prescient
  ;; Sort recent files by date
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-recentf . file-newer-than-file-p))

  (setq ivy-prescient-sort-commands
        '(:not swiper ivy-switch-buffer counsel-switch-buffer)))

(after! counsel
  (defun +counsel/counsel-compile-projectile (&optional _)
    "Add `'projectile-compilation-command to the list of compilation commands."
    (when (boundp 'projectile-project-compilation-cmd)
      projectile-project-compilation-cmd))

  (add-to-list 'counsel-compile-local-builds #'+counsel/counsel-compile-projectile))

(after! (ivy projectile counsel-projectile doct)
  (defun +counsel/counsel-switch-project-action-org-capture (project)
    (setq +counsel/counsel-switch-project-project-name (projectile-project-name project))
    (let* ((org-capture-templates (doct '((:group
                                           "Project"
                                           :template "* %doct(todo) %?"
                                           :file (lambda () (+org/project-org-file-path +counsel/counsel-switch-project-project-name))
                                           :children (("Task" :keys "p" :todo "TODO" :headline "Tasks")
                                                      ("Idea" :keys "i" :todo "IDEA" :headline "Tasks")
                                                      ("Note" :keys "n" :template "* %?" :headline "Notes")))))))
      (counsel-org-capture)))
  
  (ivy-set-actions
   'counsel-projectile-switch-project
   '(("j" counsel-projectile-switch-project-action "Jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "Jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "Jump to a project directory")
     ("D" counsel-projectile-switch-project-action-dired "Open project in dired")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer "Jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually "Find file manually from project root")
     ("S" counsel-projectile-switch-project-action-save-all-buffers "Save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers "Kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project "Remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile "Run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure "Run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals "Edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc "Open project in VC")
     ("s" counsel-projectile-switch-project-action-rg "Search project with rg")
     ("p" +counsel/counsel-switch-project-action-org-capture "Capture to project")
     ("t" counsel-projectile-switch-project-action-run-vterm "Invoke vterm from project root"))))

(after! (ivy ivy-rich counsel)
  (plist-put ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((+ivy-rich-buffer-name (:width 70))
                 (ivy-rich-switch-buffer-size (:width 7))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
                :predicate
                (lambda (cand)
                  (get-buffer cand))))

  (plist-put ivy-rich-display-transformers-list
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width 70))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
              'counsel-describe-function
              '(:columns
                ((counsel-describe-function-transformer (:width 70))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 70))
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

  (ivy-rich-reload))
