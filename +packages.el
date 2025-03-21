;;; +packages.el -*- lexical-binding: t; -*-

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

(use-package! deadgrep
  :defer t
  :commands (deadgrep--read-search-term) ;; Needed for my custom deadgrep wrapper
  :init (map! (:leader (:prefix "p" :desc "Deadgrep Project" "G" #'deadgrep)
                       (:prefix "s" :desc "Deadgrep Directory" "G" #'+bl/deadgrep-directory)))
  :config
  (set-popup-rule! "^\\*deadgrep.*$" :side 'right :size .5 :select t :quit 'current :modeline t)
  (when (modulep! :editor evil)
    (+evil-collection-init 'deadgrep))
  (map! (:map deadgrep-mode-map
              (:prefix "g"
               :n "o" #'deadgrep-visit-result
               :n "O" #'deadgrep-visit-result-other-window)
              :nv "C-j" #'deadgrep-forward-filename
              :nv "C-k" #'deadgrep-backward-filename
              :n "C-c C-e" #'wgrep-change-to-wgrep-mode
              :n [escape] #'quit-window)))

(use-package! wgrep-deadgrep
  :after deadgrep)

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
  :when (modulep! :ui treemacs)
  :init (advice-add #'treemacs-visit-node-default :around #'doom-set-jump-a)
  :config (treemacs-follow-mode +1)
  (map! :leader (:prefix "t"
                 :desc "Project Sidebar" "p" #'+treemacs/toggle)))

(use-package! pandoc-mode
  :defer t)

(use-package! yarn-mode
  :when (modulep! :lang web)
  :defer t)

(use-package! open-junk-file
  :defer t
  :init (setq open-junk-file-format (concat doom-local-dir "junk/%Y/%m/%d-%H%M%S."))
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

(use-package! auth-source-1password
  :defer t
  :init (setq auth-source-1password-vault "Development")
  :hook (doom-first-input . auth-source-1password-enable))

(use-package! hardhat
  :defer t
  :init (setq hardhat-less-feedback t)
  :hook (doom-first-input . global-hardhat-mode)
  :config (add-to-list 'hardhat-fullpath-editable-regexps "/\\.git/user/")
  (add-to-list 'hardhat-fullpath-editable-regexps ".*/\\.yas-setup.el$")
  (add-to-list 'hardhat-fullpath-editable-regexps ".*EDIT_DESCRIPTION$")
  (add-to-list 'hardhat-fullpath-editable-regexps ".*COMMIT_EDITMSG$")
  (add-to-list 'hardhat-fullpath-editable-regexps ".+/\\.git/.+/magit/posts/.+"))

(use-package! copy-as-format
  :defer t
  :init (map! (:leader (:prefix "y"
                        :desc "Copy As Format" "y" #'copy-as-format
                        :desc "Copy As Github" "g" #'copy-as-format-github
                        :desc "Copy As Slack" "s" #'copy-as-format-slack
                        :desc "Copy As Markdown" "m" #'copy-as-format-markdown
                        :desc "Copy As Org-mode" "o" #'copy-as-format-org-mode)))
  :config (setq copy-as-format-default "github"))


(use-package! subword
  :hook (prog-mode . subword-mode))

(use-package! sort-words
  :defer t
  :commands sort-words
  :init (map! (:leader (:prefix "x" :desc "Sort Words" "w" #'sort-words))))

(use-package! dired
  :defer t
  :hook (dired-mode . auto-revert-mode))

(use-package! diff-dired
  :defer t
  :commands (diff-dired-list-added diff-dired-listmodified diff-dired-list-changed)
  :when (modulep! :tools magit)
  :init (map! (:leader (:prefix "g"
                                (:prefix "l"
                                 :desc "List Added Files" "a" #'diff-dired-list-added
                                 :desc "List Modified Files" "m" #'diff-dired-list-modified
                                 :desc "List Changed Files" "c" #'diff-dired-list-changed)))))

(use-package! yaml-mode
  :defer t
  :mode (("\\.clang-format$" . yaml-mode)
         ("\\.clang-tidy$" . yaml-mode)
         ("\\.clangd$" . yaml-mode)))

(use-package! eval-sexp-fu
  :when (modulep! :lang emacs-lisp)
  :defer t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)))

(use-package! emacs-inspector
  :when (modulep! :lang emacs-lisp)
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

(use-package! dired+
  :when (modulep! :emacs dired)
  :after dired)

(use-package! ninja-mode
  :defer t)

(use-package! ff-c-style
  :defer t
  :hook (c-mode-common . ff-add-c-style))

(use-package! tspew
  :defer t
  :hook (compilation-mode . tspew-mode))

(use-package! cmake-font-lock
  :defer t)

(use-package! shader-mode
  :defer t
  :mode "\\.i?hlsl\\'")

(use-package! metal-mode
  :defer t)

(use-package! platformio-mode
  :when (executable-find "pio")
  :defer t)

(use-package! catppuccin-theme
  :defer t
  :init (setq doom-theme 'catppuccin))

(use-package! chatgpt-shell
  :defer t
  :init (setq shell-maker-history-path doom-data-dir
              chatgpt-shell-root-path doom-data-dir
              chatgpt-shell-anthropic-key (lambda () (auth-source-pick-first-password :host "Claude API" :user "password"))
              chatgpt-shell-google-key (lambda () (auth-source-pick-first-password :host "Google AI API Key" :user "password"))
              chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "OpenAI API Key" :user "password")))

  (set-popup-rule! "^\\*\\(chatgpt\\|gemini\\\|claude\\).*$" :side 'bottom :size .5 :select t :quit 'current)

  (map! :leader
        (:prefix "l"
         :desc "ChatGPT Shell" "c" #'chatgpt-shell)
        (:prefix "c"
                 (:prefix ("g" . "GPT")
                  :desc "Describe" "d" #'chatgpt-shell-describe-code
                  :desc "Explain" "e" #'chatgpt-shell-explain-code
                  :desc "Send & Review Region" "s" #'chatgpt-shell-send-and-review-region
                  :desc "Send Region" "S" #'chatgpt-shell-send-region))))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config (progn (setq copilot-idle-delay 0.5)
                 (add-to-list 'copilot-enable-predicates #'+bl/enable-copilot-p))
  :init (map! (:leader (:prefix "t" :desc "Copilot" "a" #'copilot-mode)
                       (:prefix "c"
                                (:prefix "g"
                                 :desc "Copilot Fix" "f" #'copilot-chat-fix))))
  :bind (:map copilot-completion-map
              ("M-RET" . #'copilot-accept-completion)
              ("S-M-RET" . #'copilot-accept-completion-by-word)))

(use-package! copilot-chat
  :defer t
  :init (setq copilot-chat-frontend 'org)
  (map! :leader (:prefix "l"
                 :desc "Copilot Chat" "A" #'copilot-chat-transient))
  (set-popup-rule! "^\\*Copilot Chat.*\\*$" :side 'bottom :size .5 :select t :quit 'current))

(use-package! gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :init (map! (:leader (:prefix "l"
                        :desc "GPTel" "g" #'gptel
                        :desc "GPT Menu" "G" #'gptel-menu)))
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'bottom :size .5 :select t :quit 'current))

(use-package! dall-e-shell
  :defer t
  :init (setq dall-e-shell-openai-key (lambda () (auth-source-pick-first-password :host "OpenAI API Key" :user "password")))
  (map! :leader (:prefix "l"
                 :desc "Dall-E Shell" "C" #'dall-e-shell))
  (set-popup-rule! "^\\*dall-e\\*$" :side 'bottom :size .5 :select t :quit 'current))

(use-package! mermaid-ts-mode
  :defer t
  :init (after! ob
          (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts))))

(use-package! evil-textobj-line
  :when (modulep! :editor evil)
  :after evil)

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

(use-package! lang-mode
  :defer t)

(use-package! chezmoi
  :when (executable-find "chezmoi")
  :defer t
  :init (map! :leader (:prefix "f"
                               (:prefix ("." . "chezmoi")
                                :desc "Find File" "f" #'chezmoi-find
                                :desc "Write" "w" #'chezmoi-write
                                :desc "Open Other" "o" #'chezmoi-open-other
                                :desc "Diff" "d" #'chezmoi-diff
                                :desc "Sync" "s" #'chezmoi-sync-files
                                ))))

;;GIT
(use-package! ssh-agency
  :when (featurep :system 'windows)
  :when (modulep! :tools magit))

(use-package! git-link
  :defer t
  :init (map! :leader
              (:prefix "g"
               :desc "Git Link" "w" #'git-link)))

(use-package! git-commit
  :defer t
  :when (modulep! :tools magit)
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (when (modulep! :editor evil)
    (add-hook 'git-commit-mode-hook #'evil-insert-state))
  :bind (:map git-commit-mode-map
              ([tab] . #'+bl/move-to-next-slot)))

(use-package! magit-imerge
  :when (modulep! :tools magit)
  :after magit
  :config
  (map! (:map magit-status-mode-map
              "i" #'magit-imerge
              "#" #'magit-gitignore))
  (transient-insert-suffix 'magit-dispatch "I" '("i" "iMerge" magit-imerge))
  (transient-insert-suffix 'magit-dispatch "!" '("#" "Ignore" magit-gitignore))
  (transient-append-suffix 'magit-merge "n" '("g" "iMerge" magit-imerge)))

(use-package! magit-lfs
  :when (modulep! :tools magit)
  :after magit)

(use-package! gitignore-templates
  :defer t
  :when (modulep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                       :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                       :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))

;; ORG
(use-package! ob-chatgpt-shell
  :when (modulep! :lang org)
  :defer t
  :hook (org-mode . ob-chatgpt-shell-setup))

(use-package! org-auto-tangle
  :when (modulep! :lang org)
  :defer t
  :init (setq org-auto-tangle-default t)
  :hook (org-mode . org-auto-tangle-mode))

(use-package! ob-dall-e-shell
  :when (modulep! :lang org)
  :defer t
  :hook (org-mode . ob-dall-e-shell-setup))

(use-package! org-block-capf
  :when (modulep! :lang org)
  :after org
  :config (setq org-block-capf-edit-style 'inline))

(use-package! ob-mermaid
  :defer t)

(use-package! swedish-holidays
  :when (modulep! :lang org)
  :after calendar
  :config (swedish-holidays-setup))

(use-package! demo-it
  :when (modulep! :lang org)
  :after org)

(use-package! org-expiry
  :when (modulep! :lang org)
  :after org
  :commands org-expiry-insert-expiry
  :bind (:map org-mode-map
              ("C-c C-e" . #'org-expiry-insert-expiry))
  :init (setq org-expiry-inactive-timestamps t))

(use-package! org-ql
  :when (modulep! :lang org)
  :defer t
  :commands org-ql-search
  :init (set-popup-rule! "^\\*Org QL View:" :side 'bottom :size .5 :select t :quit 'current)
  (map! (:leader (:prefix "s" :desc "Org QL Search" :ng "g" #'org-ql-search))))

(use-package! org-archive
  :when (modulep! :lang org)
  :after org
  :init (setq org-archive-location (format "%s::%s" +org/archive-file "* From %s" )
              org-refile-target-verify-function #'+org/verify-refile-target))

(use-package! org-super-agenda
  :when (modulep! :lang org)
  :after (org org-agenda)
  :init (setq org-log-done 'time
              org-log-redeadline 'time
              org-log-reschedule 'time
              org-treat-insert-todo-heading-as-state-change t
              org-agenda-skip-scheduled-if-done t
              org-agenda-skip-deadline-if-done t
              org-archive-mark-done t
              org-todo-keywords '((sequence "TODO(t)" "WORKING(w)" "BLOCKED(b)" "IDEA(i)" "|" "DONE(d)" "CANCELED(c)" "DELEGATE(D)")
                                  (sequence "[ ](T)" "[-](W)" "[?](B)" "|" "[X](D)"))
              org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) "......" "----------------"))

  (setq org-agenda-custom-commands
        '(("d" "Daily Digest"
           ((todo "" ((org-agenda-overriding-header "Top Priority")
                      (org-super-agenda-groups
                       '((:name none
                          :priority "A"
                          :order 1)
                         (:name none
                          :deadline past
                          :order 2)
                         (:name "Due Today"
                          :deadline today
                          :order 3)
                         (:discard (:anything t))))))
            (todo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:priority "A"))
                         (:discard (:deadline today))
                         (:name "TODO"
                          :todo "TODO"
                          :order 2)))))))))

  :config
  (map! (:map org-super-agenda-header-map
              "j" nil
              "k" nil))
  (shut-up (org-super-agenda-mode)))

(use-package! vulpea
  :defer t
  :when (modulep! :lang org)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(use-package! ox-gfm
  :when (modulep! :lang org)
  :after ox)

(use-package! ox-asciidoc
  :when (modulep! :lang org)
  :after ox)
