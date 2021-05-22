;;; +packages.el -*- lexical-binding: t; -*-

;;;###package
(use-package! beginend
  :defer 5
  :diminish beginend-global-mode
  :config (beginend-global-mode 1))

;;;###package
(use-package! visual-regexp-steroids
  :after visual-regexp)

;;;###package
(use-package! visual-regexp
  :defer t
  :commands (vr/replace vr/query-replace)
  :bind (([remap replace-regexp] . #'vr/replace)
         ([remap query-replace-regexp] . #'vr/query-replace)
         ("C-c r" . #'vr/replace)
         ("C-c q" . #'vr/query-replace)
         ("C-c m" . #'vr/mc-mark))
  :init (map! (:leader (:prefix "x"
                         :desc "Replace" :ng "r" #'vr/replace
                         :desc "Query Replace" :ng "q" #'vr/query-replace))))

;;;###package
(use-package! comment-dwim-2
  :defer t
  :bind ([remap comment-dwim] . #'+core/comment-uncomment-dwim))

;;;###package
(use-package! smart-backspace
  :defer t
  :commands (smart-backspace)
  :bind ([remap backward-delete-char-untabify] . #'smart-backspace))

;;;###package
(use-package! proced
   :defer t
   :commands (proced)
   :hook (proced-mode . (lambda () (proced-toggle-auto-update +1)))
   :init (map! (:leader (:prefix "a"
                          :desc "Proced" :ng "P" #'proced)))
   :config
   (setq proced-auto-update-interval 1)
   (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))

;;;###package
(use-package! which-key
  :defer t
  :init (setq which-key-sort-order 'which-key-key-order-alpha
              which-key-add-column-padding 1
              which-key-min-display-lines 6))

;;;###package
(use-package! treemacs
  :defer t
  :init (advice-add #'treemacs-visit-node-default :around #'doom-set-jump-a)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (:map treemacs-mode-map
         ("SPC" . #'treemacs-visit-node-default))
  :config (treemacs-follow-mode +1))

;;;###package
(use-package! winum
  :init (setq winum-auto-assign-0-to-minibuffer nil
              winum-auto-setup-mode-line nil
              winum-ignored-buffers '("*which-key*"))
  :config (winum-mode)
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

;; ;;;###package
;; (use-package! winner
;;   :defer 2
;;   :init
;;   (setq +core/winner-boring-buffers '("*Completions*"
;;                                    "*Compile-Log*"
;;                                    "*inferior-lisp*"
;;                                    "*Fuzzy Completions*"
;;                                    "*Apropos*"
;;                                    "*Help*"
;;                                    "*cvs*"
;;                                    "*Buffer List*"
;;                                    "*Ibuffer*"
;;                                    "*esh command on file*"
;;                                    ))
;;   :config
;;   (setq winner-boring-buffers
;;         (append winner-boring-buffers +core/winner-boring-buffers))
;;   (winner-mode t))

;;;###package
(use-package! expand-region
  :defer t
  :commands (er/expand-region)
  :init (setq expand-region-contract-fast-key "V"
              expand-region-reset-fast-key "r")
  (map! (:leader
          :desc "Expand Region" :ng "v" #'er/expand-region)))

;;;###package
(use-package! crux
  :defer t
  :commands (crux-top-join-line crux-delete-file-and-buffer crux-rename-file-and-buffer crux-find-shell-init-file
                                crux-find-user-custom-file crux-capitalize-region crux-duplicate-current-line-or-region
                                crux-duplicate-and-comment-current-line-or-region)
  :init
  (global-set-key (kbd "<A-up>") #'join-line)
  (global-set-key (kbd "<A-down>") #'crux-top-join-line)
  (map! (:leader
          (:prefix "b"
            :desc "Delete Buffer and File" :ng "K" #'crux-delete-file-and-buffer
            :desc "Rename Buffer and File" :ng "R" #'crux-rename-file-and-buffer)
          (:prefix "f"
           :desc "Find Shell init file" :ng "S" #'crux-find-shell-init-file
           :desc "Find User Custom File" :ng "C" #'crux-find-user-custom-file)
          (:prefix "x"
            :desc "Capitalize Region" :ng "C" #'crux-capitalize-region
            (:prefix ("l" . "lines")
              :desc "Duplicate Line Or Region" :ng "d" #'crux-duplicate-current-line-or-region
              :desc "Duplicate And Comment Line Or Region" :ng "D" #'crux-duplicate-and-comment-current-line-or-region)))))

;;;###package
(use-package! dash-at-point
  :defer t
  :commands (dash-at-point dash-at-point-with-docset)
  :init
  (map! (:leader
          (:prefix "d"
            :desc "Dash @ Point" :ng "d" #'dash-at-point
            :desc "Dash @ Point With Docset" :ng "D" #'dash-at-point-with-docset))))

;;;###package
(use-package! lorem-ipsum
  :defer t
  :commands (lorem-ipsum-insert-paragraphs lorem-ipsum-insert-sentences lorem-ipsum-insert-list)
  :init (map! (:leader
                (:prefix "i"
                  (:prefix ("l" . "lorem/ipsum")
                    :desc "Paragraphs" :ng "p" #'lorem-ipsum-insert-paragraphs
                    :desc "Sentences" :ng "s" #'lorem-ipsum-insert-sentences
                    :desc "List" :ng "l" #'lorem-ipsum-insert-list)))))

;;;###package
(use-package! reveal-in-osx-finder
  :when IS-MAC
  :defer t
  :commands (reveal-in-osx-finder)
  :init (map! (:leader
                (:prefix "f"
                  :desc "Reveal in Finder" :ng "F" #'reveal-in-osx-finder))))

;;;###package
(use-package! osx-dictionary
  :when IS-MAC
  :defer t
  :commands (osx-dictionary-search-word-at-point)
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("w" . "words")
                    :desc "Search Dictionary" :ng "d" #'osx-dictionary-search-word-at-point)))))

;;;###package
(use-package! string-inflection
  :defer t
  :commands (string-inflection-all-cycle string-inflection-underscore string-inflection-upcase string-inflection-kebab-case
                                         string-inflection-lower-camelcase string-inflection-camelcase)
  :bind ("M-u" . #'string-inflection-all-cycle)
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("i" . "inflection")
                    :desc "Cycle Style" :ng [tab] #'+core/inflection-cycle-dwim
                    :desc "Underscore" :ng "u" #'string-inflection-underscore
                    :desc "Uppercase" :ng "U" #'string-inflection-upcase
                    :desc "Kebab Case" :ng "k" #'string-inflection-kebab-case
                    :desc "Lower Camel Case" :ng "c" #'string-inflection-lower-camelcase
                    :desc "Camel Case" :ng "C" #'string-inflection-camelcase)))))

;;;###package
(use-package! alert
  :defer 10
  :config
  (if (executable-find "terminal-notifier")
      (setq alert-default-style 'notifier)
    (setq alert-default-style 'osx-notifier)))

;;;###package
(use-package! pandoc-mode
  :defer t)

;;;###package
(use-package! open-junk-file
  :defer t
  :init (setq open-junk-file-format (concat doom-private-dir "junk/%Y/%m/%d-%H%M%S."))
  (map! (:leader
          (:prefix "f"
            :desc "Browse Junk Files" :ng "J" #'+bl/browse-junk-files
            :desc "Open Junk File" :ng "j" #'+bl/open-junk-file))))

;;;###package
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
          :desc "Jump to Symbol" :ng "s" #'avy-goto-symbol-1)))
  :config
  (add-to-list 'avy-dispatch-alist
               '(?c . avy-comment-word)))

;;;###package
(use-package! centered-cursor-mode
  :defer t
  :commands (centered-cursor-mode)
  :init (map! (:leader (:prefix "t"
                        :desc "Centered Cursor Mode" :g "c" #'centered-cursor-mode))))

;;;###package
(use-package! super-save
  :defer 6
  :init (setq super-save-auto-save-when-idle t
              super-save-idle-duration 30
              super-save-remote-files nil
              auto-save-default nil
              super-save-exclude '("\\*.+\\*" "^[A-Z_]+$"))
  :config
  (add-to-list 'super-save-hook-triggers 'magit-status-mode-hook)
  (super-save-mode +1))

;;;###package
(use-package! counsel-doxygen-snippets
  :when (featurep! :completion ivy)
  :after counsel
  :commands counsel-doxygen-snippets
  :init (map! :leader (:prefix "i"
                       :desc "Doxygen Snippet" :ng "S" #'counsel-doxygen-snippets)))

;;;###package
(use-package! rg
  :defer t
  :commands (rg-project rg rg-dwim rg-menu rg-list-searches rg-toggle-command-hiding)
  :bind (:map rg-mode-map
         ([tab] . #'rg-toggle-command-hiding)
         ("C-c C-e" . #'wgrep-change-to-wgrep-mode)
         ("C-x C-s" . #'wgrep-finish-edit))
  :init (set-popup-rule! "^\\*rg" :side 'bottom :size 0.8 :select t :modeline t :quit t :ignore nil)
  (setq rg-align-position-numbers t
        rg-align-line-number-field-length 3
        rg-align-column-number-field-length 3
        rg-align-line-column-separator "#"
        rg-align-position-content-separator "|")
  (map! :leader
        (:prefix "p"
         :desc "Run rg in project" :ng "r" #'rg-project)
        (:prefix "s"
         :desc "Rg" :ng "d" #'rg
         :desc "Rg Dwim" :ng "D" #'rg-dwim
         :desc "Ripgrep Dispatch" :ng "r" #'rg-menu
         :desc "List Searches" :ng "R" #'rg-list-searches)))

;;;###package
(use-package! evil-collection-rg
  :when (featurep! :editor evil)
  :after (rg evil)
  :config (evil-collection-rg-setup)
  (evil-collection-define-key 'normal 'rg-mode-map
    "i" nil
    (kbd "M-j") #'rg-next-file
    (kbd "M-k") #'rg-prev-file
    (kbd "C-j") #'evil-scroll-down
    (kbd "C-k") #'evil-scroll-up))

;;;###package
(use-package! hardhat
  :defer t
  :init (setq hardhat-less-feedback t)
  :hook (doom-first-input . global-hardhat-mode))

;;;###package
(use-package! subword
  :hook (prog-mode . subword-mode))

;;;###package
(use-package! midnight
  :defer t
  :hook (doom-first-input . midnight-mode)
  :init (setq clean-buffer-list-kill-regexps '("^\\*.*\\*$")
              clean-buffer-list-kill-never-regexps '("^\\*\\(doom\\|scratch\\|Messages\\)\\*$"
                                                     "^\\*lsp.*"
                                                     "^\\*clangd.*")))
;;;###package
(use-package! dired-x
  :defer t
  :commands dired-jump
  :init (map! (:leader
               (:prefix "f"
                :desc "Find file in Dired" :ng "d" #'dired-jump))))

;;;###package
(use-package! dired
  :defer t
  :hook (dired-mode . auto-revert-mode))

;;;###package
(use-package! yaml-mode
  :mode (("\\.clang-format\\'" . yaml-mode)
         ("\\.clang-tidy\\'" . yaml-mode)))

;;;###package
(use-package! elisp-format
  :defer t
  :commands (elisp-format-region elisp-format-buffer)
  :init (map! (:localleader
                :map emacs-lisp-mode-map
                (:prefix ("=" . "format")
                  :desc "Format Region or Buffer" :ng "=" #'+elisp/format-region-or-buffer
                  :desc "Format Region" :ng "r" #'elisp-format-region
                  :desc "Format Buffer" :ng "b" #'elisp-format-buffer))))

;;;###package
(use-package! eval-sexp-fu
  :defer t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)))

;;;###package
(use-package! sh-script
  :defer t
  :init (map! (:localleader
                :map shell-mode-map
                (:prefix ("i" . "insert")
                  :desc "Case" :ng "c" #'sh-case
                  :desc "If" :ng "i" #'sh-if
                  :desc "Function" :ng "f" #'sh-function
                  :desc "For" :ng "o" #'sh-for
                  :desc "Indexed For" :ng "e" #'sh-indexed-loop
                  :desc "While" :ng "w" #'sh-while
                  :desc "Repeat" :ng "r" #'sh-repeat
                  :desc "Select" :ng "s" #'sh-select
                  :desc "Until" :ng "u" #'sh-until
                  :desc "While Getopts" :ng "g" #'sh-while-getopts))))

;;;###package
(use-package! ebib
  :defer t
  :commands (ebib)
  :init (map! (:leader
               (:prefix "a"
                :desc "Ebib" :ng "e" #'ebib))))

;;;###package
(use-package! ninja-mode
  :defer t)

;;;###package
(use-package! ff-c-style
  :hook (c-mode-common . ff-add-c-style))

;;;###package
(use-package! sourcetrail
  :defer t
  :commands (sourcetrail-send-location)
  :init (map! (:leader
               (:prefix "c"
                :desc "Send Location (SourceTrail)" :ng "u" #'sourcetrail-send-location))))

;;;###package
(use-package! evil-surround
  :when (featurep! :editor evil)
  :after evil-embrace
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

;;;###package
(use-package! lsp-treemacs
  :when (featurep! :tools lsp)
  :after lsp-mode
  :init (map! (:leader (:prefix "e"
                        :desc "All Errors" :ng "a" #'lsp-treemacs-errors-list)))
  (set-popup-rule! "^\\*LSP Error List\\*$" :size 0.5 :side 'bottom :select t :ttl nil))

;;;###package
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

;;;###package
(use-package! mu4e-views
  :when (featurep! :email mu4e)
  :after mu4e
  :if (featurep 'xwidget-internal) ;; Test if emacs is built with xwidget support
  :init (setq mu4e-views-completion-method #'ivy
              mu4e-views-default-view-method "html"
              mu4e-views-next-previous-message-behaviour #'stick-to-current-window)
  :config (mu4e-views-mu4e-use-view-msg-method "html"))

;;;###package
(use-package! mu4e-icalendar
  :when (featurep! :email mu4e)
  :after mu4e
  :config (mu4e-icalendar-setup))

;;;###package
(use-package! mu4e-compose
  :when (featurep! :email mu4e)
  :after mu4e
  :init (setq mu4e-compose-dont-reply-to-self t
              mu4e-compose-keep-self-cc nil
              mu4e-compose-complete-addresses t))

;;;###package
(use-package! mu4e-alert
  :when (featurep! :email mu4e)
  :defer t
  :init (add-hook 'doom-first-input-hook  #'mu4e-alert-enable-mode-line-display)
  :config (mu4e-alert-set-default-style 'notifier))

;;;###package
(use-package mu4e-maildirs-extension
  :when (featurep! :email mu4e)
  :defer t
  :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load)))

;;;###package
(use-package! git-commit
  :defer t
  :when (featurep! :tools magit)
  :init
  (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  :bind (:map git-commit-mode-map
        ([tab] . #'+magit/move-to-next-slot)))

;;;###package
(use-package! gitconfig-mode
  :defer t
  :when (featurep! :tools magit)
  :mode ("\.?gitaliases$" . gitconfig-mode)
  :mode ("\.?gitconfig$" . gitconfig-mode))

;;;###package
(use-package! gitignore-mode
  :defer t
  :when (featurep! :tools magit)
  :mode ("\.?gitignore$" . gitignore-mode))

;;;###package
(use-package! gitattributes-mode
  :when (featurep! :tools magit)
  :defer t)

;;;###package
(use-package magit-imerge
  :defer t
  :when (featurep! :tools magit)
  :after magit)

;;;###package
(use-package! magit-tbdiff
  :defer t
  :when (featurep! :tools magit)
  :after magit)

;;;###package
(use-package git-messenger
  :defer t
  :when (featurep! :tools magit)
  :commands (git-messenger:popup-message)
  :init (setq  git-messenger:use-magit-popup t
               git-messenger:show-detail t)
  (map! (:leader (:prefix "g" :desc "Git Messenger" :ng "M" #'git-messenger:popup-message))))

;;;###package
(use-package! git-walktree
  :defer t
  :when (featurep! :tools magit)
  :commands (git-walktree)
  :init (map! (:leader
                (:prefix "g"
                  :desc "Walk Tree" :ng "w" #'git-walktree))))
;;;###package
(use-package! gitignore-templates
  :defer t
  :when (featurep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                        :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                        :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))
