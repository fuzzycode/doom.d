;;; +packages.el -*- lexical-binding: t; -*-

;;; Added in from doom +emacs configuration
(use-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :init (map! (:leader :desc "Expand Region" "v" #'er/expand-region))
  :config
  (defadvice! doom--quit-expand-region-a (&rest _)
    "Properly abort an expand-region region."
    :before '(evil-escape doom/escape)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! visual-regexp
  :defer t
  :commands (vr/replace vr/query-replace)
  :bind (([remap replace-regexp] . #'vr/replace)
         ([remap query-replace-regexp] . #'vr/query-replace)
         ([remap isearch-forward] . #'vr/isearch-forward)
         ([remap isearch-backward] . #'vr/isearch-backward)
         ("C-c r" . #'vr/replace)
         ("C-c q" . #'vr/query-replace)
         ("C-c m" . #'vr/mc-mark))
  :init (map! (:leader (:prefix "s"
                        :desc "Replace" :ng "q" #'vr/replace
                        :desc "Query Replace" :ng "Q" #'vr/query-replace))))

(use-package! graphql-mode
  :defer t)

(use-package! ialign
  :defer t
  :commands ialign
  :init (map! (:leader (:prefix "x" :desc "Align" "a" #'ialign))))

(use-package! smart-backspace
  :defer t
  :commands (smart-backspace)
  :bind ([remap backward-delete-char-untabify] . #'smart-backspace))

(use-package! treemacs
  :defer t
  :when (featurep! :ui treemacs)
  :init (advice-add #'treemacs-visit-node-default :around #'doom-set-jump-a)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (:map treemacs-mode-map
         ("SPC" . #'treemacs-visit-node-default))
  :config (treemacs-follow-mode +1))

(use-package! pandoc-mode
  :defer t)

(use-package! yarn-mode
  :when (featurep! :lang web)
  :defer t)

(use-package! open-junk-file
  :defer t
  :init (setq open-junk-file-format (concat doom-private-dir "junk/%Y/%m/%d-%H%M%S."))
  (map! (:leader
         (:prefix "f"
          :desc "Browse Junk Files" :ng "J" #'+bl/browse-junk-files
          :desc "Open Junk File" :ng "j" #'open-junk-file))))

(use-package! centered-cursor-mode
  :defer t
  :commands (centered-cursor-mode)
  :init (map! (:leader (:prefix "t"
                        :desc "Centered Cursor Mode" "C" #'centered-cursor-mode))))

(use-package! ssh-config-mode
  :defer t)

(use-package! hardhat
  :defer t
  :init (setq hardhat-less-feedback t)
  :hook (doom-first-input . global-hardhat-mode)
  :config (add-to-list 'hardhat-fullpath-editable-regexps "/\\.git/user/")
  (add-to-list 'hardhat-fullpath-editable-regexps ".*COMMIT_EDITMSG$")
  (add-to-list 'hardhat-fullpath-editable-regexps ".+/\\.git/.+/magit/posts/.+"))

(use-package! subword
  :hook (prog-mode . subword-mode))

(use-package! sort-words
  :defer t
  :commands sort-words
  :init (map! (:leader (:prefix "x" :desc "Sort Words" "w" #'sort-words))))

(use-package! dired
  :defer t
  :hook (dired-mode . auto-revert-mode))

(use-package! yaml-mode
  :defer t
  :mode (("\\.clang-format$" . yaml-mode)
         ("\\.clang-tidy$" . yaml-mode)
         ("\\.clangd$" . yaml-mode)))

(use-package! eval-sexp-fu
  :when (featurep! :lang emacs-lisp)
  :defer t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)))

(use-package! emacs-inspector
  :when (featurep! :lang emacs-lisp)
  :defer t
  :init (map! (:localleader
               :map (emacs-lisp-mode-map lisp-interaction-mode-map)
               (:prefix ("i" . "inspect")
                :desc "Inspect Last Sexp" "i" #'inspect-last-sexp
                :desc "Inspect Expression" "e" #'inspect-expression))
              (:map inspector-mode-map
               "q" #'inspector-pop
               [escape] #'inspector-quit
               "<tab>" #'forward-button
               "S-<tab>" #'backward-button)))

(use-package! vundo
  :unless (featurep! +tree)
  :custom
  (vundo-glyph-alist     vundo-unicode-symbols)
  (vundo-compact-display t)
  :config
  (map! (:leader :desc "Visual Undo" "U" #'vundo))
  (when (featurep! :editor evil)
    (set-evil-initial-state! 'vundo-mode 'motion)
    (add-hook! vundo-mode #'evil-normalize-keymaps)
    (map! :map vundo-mode-map
          [remap evil-backward-char] #'vundo-backward
          [remap evil-forward-char]  #'vundo-forward
          [remap evil-next-line]     #'vundo-next
          [remap evil-previous-line] #'vundo-previous
          [remap evil-window-top]    #'vundo-stem-root
          [remap evil-window-bottom] #'vundo-stem-end
          "q"                        #'vundo-quit
          [escape]                   #'vundo-quit
          [remap evil-ret]           #'vundo-confirm))
  :defer t)

(use-package! ninja-mode
  :defer t)

(use-package! ff-c-style
  :defer t
  :hook (c-mode-common . ff-add-c-style))

(use-package! shader-mode
  :defer t
  :mode "\\.i?hlsl\\'")

(use-package! evil-textobj-line
  :when (featurep! :editor evil)
  :after evil)

(use-package! mu4e-views
  :when (featurep! :email mu4e)
  :after mu4e
  :if (featurep 'xwidget-internal) ;; Test if emacs is built with xwidget support
  :init (setq mu4e-views-default-view-method "html"
              mu4e-views-next-previous-message-behaviour #'stick-to-current-window)
  :config (mu4e-views-mu4e-use-view-msg-method "html"))

(use-package mu4e-maildirs-extension
  :when (featurep! :email mu4e)
  :after mu4e
  :config (mu4e-maildirs-extension-load))

(use-package! fix-word
  :defer t
  :init (map! (:leader
               (:prefix "x"
                :desc "Upcase" "u" #'fix-word-upcase
                :desc "Downcase" "d" #'fix-word-downcase
                :desc "Capitalize" "c" #'fix-word-capitalize))))

(use-package! string-inflection
  :defer t
  :init (map! (:leader
               (:prefix "x"
                :desc "Inflection" "i" #'+bl/string-inflection-cycle-dwim))))

(use-package! yasnippet
  :defer t
  :config (when (file-directory-p "~/.snippets")
            (add-to-list 'yas-snippet-dirs "~/.snippets")
            (yas-reload-all)))

(use-package! smart-newline
  :defer t
  :hook (doom-first-input . smart-newline-mode))

(use-package! ssh-agency
  :when (featurep! :tools magit))

(use-package! git-commit
  :defer t
  :when (featurep! :tools magit)
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (when (featurep! :editor evil)
    (add-hook 'git-commit-mode-hook #'evil-insert-state))
  :bind (:map git-commit-mode-map
         ([tab] . #'+bl/move-to-next-slot)))

(use-package! magit-imerge
  :when (featurep! :tools magit)
  :after magit
  :config
  (map! (:map magit-status-mode-map
         "i" #'magit-imerge
         "#" #'magit-gitignore))
  (transient-insert-suffix 'magit-dispatch "I" '("i" "iMerge" magit-imerge))
  (transient-insert-suffix 'magit-dispatch "!" '("#" "Ignore" magit-gitignore))
  (transient-append-suffix 'magit-merge "n" '("g" "iMerge" magit-imerge)))

(use-package! gitignore-templates
  :defer t
  :when (featurep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                       :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                       :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))

;; ORG
(use-package! swedish-holidays
  :when (featurep! :lang org)
  :after calendar
  :config (swedish-holidays-setup))

(use-package! demo-it
  :when (featurep! :lang org)
  :after org)

(use-package! org-expiry
  :when (featurep! :lang org)
  :after org
  :commands org-expiry-insert-expiry
  :bind (:map org-mode-map
         ("C-c C-e" . #'org-expiry-insert-expiry))
  :init (setq org-expiry-inactive-timestamps t))

(use-package! org-ql
  :when (featurep! :lang org)
  :defer t
  :commands org-ql-search
  :init (set-popup-rule! "^\\*Org QL View:" :side 'bottom :size .5 :select t :quit 'current)
  (map! (:leader (:prefix "s" :desc "Org QL Search" :ng "g" #'org-ql-search))))

(use-package! org-appear
  :when (featurep! :lang org)
  :after org
  :init (setq org-appear-delay 0.3
              org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package! org-archive
  :when (featurep! :lang org)
  :after org
  :init (setq org-archive-location (format "%s::%s" +org/archive-file "* From %s" )
              org-refile-target-verify-function #'+org/verify-refile-target))

(use-package! org-super-agenda
  :when (featurep! :lang org)
  :after (org org-agenda)
  :init (setq org-super-agenda-groups '((:name "Today"
                                         :scheduled today)
                                        (:name "Overdue"
                                         :scheduled past)
                                        (:name "Soon"
                                         :scheduled future)))
  :config
  ;; TODO: Only clear hjkl keys needed for navigation
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (shut-up (org-super-agenda-mode)))

(use-package! doct
  :when (featurep! :lang org)
  :commands doct)

(use-package! ox-gfm
  :when (featurep! :lang org)
  :after ox)

(use-package! ox-asciidoc
  :when (featurep! :lang org)
  :after ox)
