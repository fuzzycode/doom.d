;;; ~/.doom.d/modules/private/core/config.el -*- lexical-binding: t; -*-

(load! "+functions")

;;;###package
(use-package! use-package-chords)

;;;###package
(use-package! paradox
  :defer t
  :init (map! (:leader (:prefix "a"
                         :desc "Paradox List Packages" :g "l" #'paradox-list-packages)))
  (setq paradox-column-width-package 30
        paradox-column-width-version 15))

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
  :bind (([remap replace-regexp] . #'vr/replace)
         ([remap query-replace-regexp] . #'vr/query-replace)
         ("C-c r" . #'vr/replace)
         ("C-c q" . #'vr/query-replace)
         ("C-c m" . #'vr/mc-mark))
  :init (map! (:leader (:prefix ("x" . "text")
                         :desc "Replace" :g "r" #'vr/replace
                         :desc "Query Replace" :g "q" #'vr/query-replace))))

;;;###package
(use-package! comment-dwim-2
  :bind ([remap comment-dwim] . #'+core/comment-uncomment-dwim))

;;;###package
(use-package! smart-backspace
  :bind ([remap backward-delete-char-untabify] . #'smart-backspace))

;;;###package
(use-package! proced
   :defer t
   :hook (proced-mode . (lambda () (proced-toggle-auto-update +1)))
   :init (map! (:leader (:prefix ("a" . "applications")
                          :desc "Proced" :g "P" #'proced)))
   :config
   (setq proced-auto-update-interval 1)
   (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))

;;;###package
(use-package! mwim
  :bind (("C-a" . #'mwim-beginning-of-code-or-line)
         ("C-e" . #'mwim-end-of-code-or-line)))

;;;###package
(use-package! which-key
  :init (setq which-key-sort-order 'which-key-key-order-alpha
              which-key-add-column-padding 1
              which-key-min-display-lines 6))

;;;###package
(use-package! treemacs
  :init (advice-add #'treemacs-visit-node-default :around #'doom-set-jump-a)
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

;;;###package
(use-package! winner
  :defer 2
  :init
  (setq +core/winner-boring-buffers '("*Completions*"
                                   "*Compile-Log*"
                                   "*inferior-lisp*"
                                   "*Fuzzy Completions*"
                                   "*Apropos*"
                                   "*Help*"
                                   "*cvs*"
                                   "*Buffer List*"
                                   "*Ibuffer*"
                                   "*esh command on file*"
                                   ))
  :config
  (setq winner-boring-buffers
        (append winner-boring-buffers +core/winner-boring-buffers))
  (winner-mode t))

;;;###package
(use-package! expand-region
  :defer t
  :init (setq expand-region-contract-fast-key "V"
              expand-region-reset-fast-key "r")
  (map! (:leader
          :desc "Expand Region" :g "v" #'er/expand-region)))

;;;###package
(use-package! insert-shebang
  :defer t
  :init
  (map! (:leader (:prefix "i" :desc "Insert shebang" :g "!" #'insert-shebang)))
  (remove-hook 'find-file-hook #'insert-shebang))

;;;###package
(use-package! crux
  :defer t
  :commands crux-top-join-line
  :init
  (global-set-key (kbd "<A-up>") #'join-line)
  (global-set-key (kbd "<A-down>") #'crux-top-join-line)
  (map! (:leader
          (:prefix "b"
            :desc "Delete Buffer and File" :g "K" #'crux-delete-file-and-buffer
            :desc "Rename Buffer and File" :g "R" #'crux-rename-file-and-buffer)
          (:prefix "f"
           :desc "Find Shell init file" :g "S" #'crux-find-shell-init-file
           :desc "Find User Custom File" :g "C" #'crux-find-user-custom-file)
          (:prefix "x"
            :desc "Capitalize Region" :g "C" #'crux-capitalize-region
            (:prefix ("l" . "lines")
              :desc "Duplicate Line Or Region" :g "d" #'crux-duplicate-current-line-or-region
              :desc "Duplicate And Comment Line Or Region" :g "D" #'crux-duplicate-and-comment-current-line-or-region)))))

;;;###package
(use-package! dash-at-point
  :defer t
  :init
  (map! (:leader
          (:prefix ("d" . "documentation")
            :desc "Dash @ Point" :g "d" #'dash-at-point
            :desc "Dash @ Point With Docset" :g "D" #'dash-at-point-with-docset))))

;;;###package
(use-package! helm-dash
  :when (featurep! :completion helm)
  :defer t
  :init (map! (:leader
               (:prefix ("d" . "documentation")
                 :desc "Helm Dash @ Point" :g "h" #'helm-dash-at-point
                 :desc "Helm Dash" :g "H" #'helm-dash))))

;;;###package
(use-package! lorem-ipsum
  :defer t
  :init (map! (:leader
                (:prefix "i"
                  (:prefix ("l" . "lorem/ipsum")
                    :desc "Paragraphs" :g "p" #'lorem-ipsum-insert-paragraphs
                    :desc "Sentences" :g "s" #'lorem-ipsum-insert-sentences
                    :desc "List" :g "l" #'lorem-ipsum-insert-list)))))

;;;###package
(use-package! reveal-in-osx-finder
  :when IS-MAC
  :defer t
  :init (map! (:leader
                (:prefix "f"
                  :desc "Reveal in Finder" :g "F" #'reveal-in-osx-finder))))

;;;###package
(use-package! osx-dictionary
  :when IS-MAC
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("w" . "words")
                    :desc "Search Dictionary" :g "d" #'osx-dictionary-search-word-at-point)))))

;;;###package
(use-package! string-inflection
  :defer t
  :bind ("M-u" . #'string-inflection-all-cycle)
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("i" . "inflection")
                    :desc "Cycle Style" :g [tab] #'+core/inflection-cycle-dwim
                    :desc "Underscore" :g "u" #'string-inflection-underscore
                    :desc "Uppercase" :g "U" #'string-inflection-upcase
                    :desc "Kebab Case" :g "k" #'string-inflection-kebab-case
                    :desc "Lower Camel Case" :g "c" #'string-inflection-lower-camelcase
                    :desc "Camel Case" :g "C" #'string-inflection-camelcase)))))

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
            :desc "Browse Junk Files" :g "J" #'+core/browse-junk-files
            :desc "Open Junk File" :g "j" #'+core/open-junk-file))))

;;;###package
(use-package! key-chord
  :defer 2
  :init
  (key-chord-define-global "uu" #'undo-fu-only-undo)
  (key-chord-define-global "kk" #'just-one-space)
  :config (shut-up (key-chord-mode +1)))

;;;###package
(use-package! avy
  :commands (avy-goto-word-or-subword-1 avy-goto-line avy-goto-char-timer)
  :init
  ;; Integrate avy with better-jumper, might be a better way to cover all avy jump functions
  (advice-add #'avy-goto-word-or-subword-1 :around #'doom-set-jump-a)
  (advice-add #'avy-goto-char-timer :around #'doom-set-jump-a)
  (advice-add #'avy-goto-line :around #'doom-set-jump-a)

  (key-chord-define-global "jj" #'avy-goto-word-or-subword-1)
  (map! (:leader
         (:prefix "j"
          :desc "Jump to Word" :g "j" #'avy-goto-char-timer
          :desc "Jump to Line" :g "l" #'avy-goto-line
          :desc "Jump to Symbol" :g "s" #'avy-goto-symbol-1)))
  :config
  (add-to-list 'avy-dispatch-alist
               '(?c . avy-comment-word)))

;;;###package
(use-package! goto-last-change
  :defer t
  :commands goto-last-change
  :init (map! (:leader (:prefix "j"
                         :desc "Goto Last Change" :g "c" #'goto-last-change))))

;;;###package
(use-package! buffer-flip
  :init (map! (:leader :desc "Flip Buffer" :g [tab] #'buffer-flip))
  :chords (("u8" . buffer-flip))
  :bind  (:map buffer-flip-map
           ([tab] . buffer-flip-backward)
           ("8" .   buffer-flip-forward)
           ("*" .   buffer-flip-backward)
           ("C-g" . buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*.*\\*$"
          "^magit-diff.*$"
          "^magit-process.*$")))

;;;###package
(use-package! centered-cursor-mode
  :defer t
  :commands centered-cursor-mode
  :init (+core/add-toggle centered-cursor-mode :mode centered-cursor-mode :bind '(:desc "Centered Cursor Mode" :key "c"))
  )

;;;###package
(use-package! highlight-doxygen
  :disabled
  :after hl-line
  :hook (doom-load-theme . (lambda () (set-face-background 'highlight-doxygen-comment (face-background 'hl-line))))
  :init (+core/add-toggle highlight-doxygen-mode :mode highlight-doxygen-mode :bind '(:desc "Highlight Doxygen" :key "d"))
  (after! wucuo
    (add-to-list 'wucuo-personal-font-faces-to-check 'highlight-doxygen-comment))
  :config (highlight-doxygen-global-mode 1))

;;;###package
(use-package! easy-kill
  :commands easy-kill
  :bind (([remap kill-ring-save] . #'easy-kill)))

;;;###package
(use-package! super-save
  :defer 6
  :init (setq super-save-auto-save-when-idle nil
              super-save-idle-duration 30
              super-save-remote-files nil)
  :config
  (setq super-save-triggers (append super-save-triggers
                                    '(+ivy/switch-buffer +ivy/switch-workspace-buffer
                                                         +ivy/switch-buffer-other-window +ivy/switch-workspace-buffer-other-window
                                                         ace-window winum-select-window-0 winum-select-window-1 winum-select-window-2
                                                         winum-select-window-3 winum-select-window-4 winum-select-window-5
                                                         winum-select-window-6 winum-select-window-7 winum-select-window-8
                                                         winum-select-window-9 winum-select-window-0-or-10)))
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (setq auto-save-default nil)
  (super-save-mode +1))

;;;###package
(use-package! counsel-doxygen-snippets
  :when (featurep! :completion ivy)
  :after counsel
  :commands counsel-doxygen-snippets
  :init (map! :leader (:prefix "i"
                       :desc "Doxygen Snippet" :g "S" #'counsel-doxygen-snippets)))

;;;###package
(use-package! flycheck-clazy
  :when (featurep! :checkers syntax)
  :after flycheck)

;;;###package
(use-package! uuidgen
  :commands (uuidgen-1 uuidgen-4)
  :init (map! :leader (:prefix "i"
                        (:prefix ("U" . "uuid")
                         :desc "Time based UUID (1)" :g "t" (cmd! ()  (insert (uuidgen-1)))
                         :desc "Random based UUID (4)" :g "r" (cmd! () (insert (uuidgen-4)))
                         :desc "UUID" :g "U" (cmd! () (insert (uuidgen-4)))))))

;;;###package
(use-package! smart-newline
  :defer 1
  :bind ("RET" . #'smart-newline))

;;;###package
(use-package! rg
  :defer 2
  :init (set-popup-rule! "^\\*rg" :side 'bottom :size 0.8 :select t :modeline t :quit t :ignore nil)
  (setq rg-align-position-numbers t
        rg-align-line-number-field-length 3
        rg-align-column-number-field-length 3
        rg-align-line-column-separator "#"
        rg-align-position-content-separator "|")
  :bind (:map rg-mode-map
          ("TAB" . #'rg-toggle-command-hiding)
          ("C-p" . #'previous-line)
          ("C-n" . #'next-line)
          ("M-n" . #'rg-next-file)
          ("M-p" . #'rg-prev-file))
  :config (map! :leader
                (:prefix "p"
                  :desc "Run rg in project" :g "r" #'rg-project)
                (:prefix "s"
                  :desc "Rg" :g "d" #'rg
                  :desc "Rg Dwim" :g "D" #'rg-dwim
                  :desc "Ripgrep Dispatch" :g "r" #'rg-menu
                  :desc "List Searches" :g "R" #'rg-list-searches)))

;;;###package
(use-package! hardhat
  :defer t
  :init (setq hardhat-less-feedback t)
  :hook (doom-first-input . global-hardhat-mode))

;; EXTRAS
(load! "+configs")
(load! "+bindings")
(load! "+popups")
