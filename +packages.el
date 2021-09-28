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

(use-package! winum
  :defer t
  :hook (doom-first-input . winum-mode)
  :init (setq winum-auto-assign-0-to-minibuffer nil
              winum-auto-setup-mode-line nil
              winum-ignored-buffers '("*which-key*"))
  :bind (:map winum-keymap
         ("M-1" . #'winum-select-window-1)
         ("M-2" . #'winum-select-window-2)
         ("M-3" . #'winum-select-window-3)
         ("M-4" . #'winum-select-window-4)
         ("M-5" . #'winum-select-window-5)
         ("M-6" . #'winum-select-window-6)
         ("M-7" . #'winum-select-window-7)
         ("M-8" . #'winum-select-window-8)
         ("M-9" . #'winum-select-window-9)))

(use-package! pandoc-mode
  :defer t)

(use-package! open-junk-file
  :defer t
  :init (setq open-junk-file-format (concat doom-private-dir "junk/%Y/%m/%d-%H%M%S."))
  (map! (:leader
         (:prefix "f"
          :desc "Browse Junk Files" :ng "J" #'+bl/browse-junk-files
          :desc "Open Junk File" :ng "j" #'open-junk-file))))

(use-package! avy
  :commands (avy-goto-word-or-subword-1 avy-goto-line avy-goto-char-timer)
  :init
  ;; Integrate avy with better-jumper, might be a better way to cover all avy jump functions
  (advice-add #'avy-goto-word-or-subword-1 :around #'doom-set-jump-a)
  (advice-add #'avy-goto-char-timer :around #'doom-set-jump-a)
  (advice-add #'avy-goto-line :around #'doom-set-jump-a)

  (map! (:leader
         (:prefix "j"
          :desc "Jump to Word" :ng "j" #'avy-goto-char-timer
          :desc "Jump to Line" :ng "l" #'avy-goto-line
          :desc "Jump to Symbol" :ng "s" #'avy-goto-symbol-1))
        :nv "gb" #'avy-goto-char-timer))

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
  :hook (doom-first-input . global-hardhat-mode))

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
  :defer t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)))

(use-package! ninja-mode
  :defer t)

(use-package! ff-c-style
  :defer t
  :hook (c-mode-common . ff-add-c-style))

(use-package! sourcetrail
  :defer t
  :commands (sourcetrail-send-location)
  :init (map! (:leader
               (:prefix "c"
                :desc "Send Location (SourceTrail)" :ng "u" #'sourcetrail-send-location))))

(use-package! evil-surround
  :when (featurep! :editor evil)
  :after evil-embrace
  :init (add-hook 'c++-mode-hook (lambda ()
                                   (push '(?* . ("/*" . "*/")) evil-surround-pairs-alist)))
  :config
  (evil-define-key 'visual evil-surround-mode-map "s" #'evil-surround-region)
  (let ((pairs '((?g "$" . "$")
                 (?h "(" . ")")
                 (?j "[" . "]")
                 (?k "{" . "}")
                 (?l "<" . ">")
                 (?a "'" . "'")
                 (?s "\"" . "\""))))
    (setq-default evil-embrace-evil-surround-keys (append evil-embrace-evil-surround-keys (mapcar #'car pairs)))
    (setq-default evil-surround-pairs-alist (append evil-surround-pairs-alist pairs))))

(use-package! evil-textobj-line
  :when (featurep! :editor evil)
  :after evil)

(use-package! mu4e
  :when (featurep! :email mu4e)
  :defer t
  :init (setq mu4e-root-maildir (expand-file-name "~/.mail")
              mu4e-drafts-folder "/drafts"
              mu4e-sent-folder   "/sent"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive"
              mu4e-update-interval 120
              mu4e-headers-auto-update t
              mu4e-headers-date-format "%Y-%m-%d %H:%M"
              mu4e-change-filenames-when-moving t
              mu4e-sent-messages-behavior 'delete
              mu4e-use-fancy-chars nil
              mu4e-headers-fields `((:human-date . 18)
                                    (:flags . 4)
                                    (:from-or-to . 20)
                                    (:subject))))

(use-package! mu4e-views
  :when (featurep! :email mu4e)
  :after mu4e
  :if (featurep 'xwidget-internal) ;; Test if emacs is built with xwidget support
  :init (setq mu4e-views-default-view-method "html"
              mu4e-views-next-previous-message-behaviour #'stick-to-current-window)
  :config (mu4e-views-mu4e-use-view-msg-method "html"))

(use-package! mu4e-icalendar
  :when (featurep! :email mu4e)
  :after mu4e
  :config (mu4e-icalendar-setup))

(use-package! mu4e-compose
  :when (featurep! :email mu4e)
  :after mu4e
  :init (setq mu4e-compose-dont-reply-to-self t
              mu4e-compose-keep-self-cc nil
              mu4e-compose-complete-addresses t))

(use-package! mu4e-alert
  :when (featurep! :email mu4e)
  :after mu4e
  :init (setq mu4e-alert-interesting-mail-query (mapconcat #'identity '("flag:unread" "AND NOT" "flag:trashed" "AND" "maildir:/inbox") " "))
  :config (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-mode-line-display))

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

(use-package! git-commit
  :defer t
  :when (featurep! :tools magit)
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (when (featurep! :editor evil)
    (add-hook 'git-commit-mode-hook #'evil-insert-state))
  :bind (:map git-commit-mode-map
         ([tab] . #'+bl/move-to-next-slot)))

(use-package! gitconfig-mode
  :defer t
  :when (featurep! :tools magit)
  :mode ("\.?gitaliases$" . gitconfig-mode)
  :mode ("\.?gitconfig$" . gitconfig-mode))

(use-package! gitignore-mode
  :defer t
  :when (featurep! :tools magit)
  :mode ("\\.?\\(fd\\|git\\)?ignore$" . gitignore-mode))

(use-package! gitattributes-mode
  :when (featurep! :tools magit)
  :defer t)

(use-package! magit-imerge
  :defer t
  :when (featurep! :tools magit)
  :after magit)

(use-package! gitignore-templates
  :defer t
  :when (featurep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                       :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                       :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))

;; ORG

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
  :after ox)

(use-package! ox-asciidoc
  :after ox)
