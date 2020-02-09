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


(after! (ivy ivy-rich counsel)
  (plist-put ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((+ivy-rich-buffer-name (:width 70))
                 (ivy-rich-switch-buffer-size (:width 7))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
                 (ivy-rich-switch-buffer-project (:width 20 :face success))
                 (ivy-rich-switch-buffer-path (:width
                                               (lambda (x)
                                                 (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
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
