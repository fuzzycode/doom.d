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
                         :desc "Replace" :nvg "r" #'vr/replace
                         :desc "Query Replace" :nvg "q" #'vr/query-replace))))

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
                          :desc "Proced" :nvg "P" #'proced)))
   :config
   (setq proced-auto-update-interval 1)
   (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))

;; ;;;###package
(use-package! which-key
  :defer t
  :init (setq which-key-sort-order 'which-key-key-order-alpha
              which-key-add-column-padding 1
              which-key-min-display-lines 6))

;;;###package
(use-package! treemacs
  :defer t
  :init (advice-add #'treemacs-visit-node-default :around #'doom-set-jump-a)
  :bind (:map treemacs-mode-map
         ("SPC" . #'treemacs-visit-node-default))
  :config (treemacs-follow-mode +1))

;; ;;;###package
;; (use-package! winum
;;   :init (setq winum-auto-assign-0-to-minibuffer nil
;;               winum-auto-setup-mode-line nil
;;               winum-ignored-buffers '("*which-key*"))
;;   :config (winum-mode)
;;   :bind (:map winum-keymap
;;           ("M-1" . #'winum-select-window-1)
;;           ("M-2" . #'winum-select-window-2)
;;           ("M-3" . #'winum-select-window-3)
;;           ("M-4" . #'winum-select-window-4)
;;           ("M-5" . #'winum-select-window-5)
;;           ("M-6" . #'winum-select-window-6)
;;           ("M-7" . #'winum-select-window-7)
;;           ("M-8" . #'winum-select-window-8)
;;           ("M-9" . #'winum-select-window-9)))

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
          :desc "Expand Region" :nvg "v" #'er/expand-region)))

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
            :desc "Delete Buffer and File" :nvg "K" #'crux-delete-file-and-buffer
            :desc "Rename Buffer and File" :nvg "R" #'crux-rename-file-and-buffer)
          (:prefix "f"
           :desc "Find Shell init file" :nvg "S" #'crux-find-shell-init-file
           :desc "Find User Custom File" :nvg "C" #'crux-find-user-custom-file)
          (:prefix "x"
            :desc "Capitalize Region" :nvg "C" #'crux-capitalize-region
            (:prefix ("l" . "lines")
              :desc "Duplicate Line Or Region" :nvg "d" #'crux-duplicate-current-line-or-region
              :desc "Duplicate And Comment Line Or Region" :nvg "D" #'crux-duplicate-and-comment-current-line-or-region)))))

;; ;;;###package
(use-package! dash-at-point
  :defer t
  :commands (dash-at-point dash-at-point-with-docset)
  :init
  (map! (:leader
          (:prefix "d"
            :desc "Dash @ Point" :nvg "d" #'dash-at-point
            :desc "Dash @ Point With Docset" :nvg "D" #'dash-at-point-with-docset))))

;;;###package
(use-package! lorem-ipsum
  :defer t
  :commands (lorem-ipsum-insert-paragraphs lorem-ipsum-insert-sentences lorem-ipsum-insert-list)
  :init (map! (:leader
                (:prefix "i"
                  (:prefix ("l" . "lorem/ipsum")
                    :desc "Paragraphs" :nvg "p" #'lorem-ipsum-insert-paragraphs
                    :desc "Sentences" :nvg "s" #'lorem-ipsum-insert-sentences
                    :desc "List" :nvg "l" #'lorem-ipsum-insert-list)))))

;; ;;;###package
(use-package! reveal-in-osx-finder
  :when IS-MAC
  :defer t
  :commands (reveal-in-osx-finder)
  :init (map! (:leader
                (:prefix "f"
                  :desc "Reveal in Finder" :nvg "F" #'reveal-in-osx-finder))))

;; ;;;###package
(use-package! osx-dictionary
  :when IS-MAC
  :defer t
  :commands (osx-dictionary-search-word-at-point)
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("w" . "words")
                    :desc "Search Dictionary" :nvg "d" #'osx-dictionary-search-word-at-point)))))

;;;###package
(use-package! string-inflection
  :defer t
  :commands (string-inflection-all-cycle string-inflection-underscore string-inflection-upcase string-inflection-kebab-case
                                         string-inflection-lower-camelcase string-inflection-camelcase)
  :bind ("M-u" . #'string-inflection-all-cycle)
  :init (map! (:leader
                (:prefix "x"
                  (:prefix ("i" . "inflection")
                    :desc "Cycle Style" :nvg [tab] #'+core/inflection-cycle-dwim
                    :desc "Underscore" :nvg "u" #'string-inflection-underscore
                    :desc "Uppercase" :nvg "U" #'string-inflection-upcase
                    :desc "Kebab Case" :nvg "k" #'string-inflection-kebab-case
                    :desc "Lower Camel Case" :nvg "c" #'string-inflection-lower-camelcase
                    :desc "Camel Case" :nvg "C" #'string-inflection-camelcase)))))

;; ;;;###package
(use-package! alert
  :defer 10
  :config
  (if (executable-find "terminal-notifier")
      (setq alert-default-style 'notifier)
    (setq alert-default-style 'osx-notifier)))

;;;###package
(use-package! pandoc-mode
  :defer t)

;; ;;;###package
(use-package! open-junk-file
  :defer t
  :init (setq open-junk-file-format (concat doom-private-dir "junk/%Y/%m/%d-%H%M%S."))
  (map! (:leader
          (:prefix "f"
            :desc "Browse Junk Files" :nvg "J" #'+core/browse-junk-files
            :desc "Open Junk File" :nvg "j" #'+core/open-junk-file))))

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
          :desc "Jump to Word" :nvg "j" #'avy-goto-char-timer
          :desc "Jump to Line" :nvg "l" #'avy-goto-line
          :desc "Jump to Symbol" :nvg "s" #'avy-goto-symbol-1)))
  :config
  (add-to-list 'avy-dispatch-alist
               '(?c . avy-comment-word)))

;;;###package
(use-package! buffer-flip
  :defer t
  :commands (buffer-flip)
  :init (map! (:leader :desc "Flip Buffer" :nvg [tab] #'buffer-flip))
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

;; ;;;###package
(use-package! centered-cursor-mode
  :defer t
  :commands (centered-cursor-mode)
  :init (+core/add-toggle centered-cursor-mode :mode centered-cursor-mode :bind '(:desc "Centered Cursor Mode" :key "c")))


;; ;;;###package
;; (use-package! easy-kill
;;   :commands easy-kill
;;   :bind (([remap kill-ring-save] . #'easy-kill)))

;; ;;;###package
;; (use-package! super-save
;;   :defer 6
;;   :init (setq super-save-auto-save-when-idle nil
;;               super-save-idle-duration 30
;;               super-save-remote-files nil
;;               super-save-exclude '("\\*.+\\*"))
;;   :config
;;   (setq super-save-triggers (append super-save-triggers
;;                                     '(+ivy/switch-buffer +ivy/switch-workspace-buffer
;;                                                          +ivy/switch-buffer-other-window +ivy/switch-workspace-buffer-other-window
;;                                                          ace-window winum-select-window-0 winum-select-window-1 winum-select-window-2
;;                                                          winum-select-window-3 winum-select-window-4 winum-select-window-5
;;                                                          winum-select-window-6 winum-select-window-7 winum-select-window-8
;;                                                          winum-select-window-9 winum-select-window-0-or-10)))
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook)
;;   (setq auto-save-default nil)
;;   (super-save-mode +1))

;;;###package
(use-package! counsel-doxygen-snippets
  :when (featurep! :completion ivy)
  :after counsel
  :commands counsel-doxygen-snippets
  :init (map! :leader (:prefix "i"
                       :desc "Doxygen Snippet" :nvg "S" #'counsel-doxygen-snippets)))

;;;###package
(use-package! flycheck-clazy
  :when (featurep! :checkers syntax)
  :after flycheck)

;;;###package
(use-package! uuidgen
  :defer t
  :commands (uuidgen-1 uuidgen-4)
  :init (map! :leader (:prefix "i"
                        (:prefix ("U" . "uuid")
                         :desc "Time based UUID (1)" :nvg "t" (cmd! ()  (insert (uuidgen-1)))
                         :desc "Random based UUID (4)" :nvg "r" (cmd! () (insert (uuidgen-4)))
                         :desc "UUID" :nvg "U" (cmd! () (insert (uuidgen-4)))))))

;; ###package
(use-package! rg
  :defer t
  :commands (rg-project rg rg-dwim rg-menu rg-list-searches)
  :init (set-popup-rule! "^\\*rg" :side 'bottom :size 0.8 :select t :modeline t :quit t :ignore nil)
  (setq rg-align-position-numbers t
        rg-align-line-number-field-length 3
        rg-align-column-number-field-length 3
        rg-align-line-column-separator "#"
        rg-align-position-content-separator "|")
  (map! :leader
        (:prefix "p"
         :desc "Run rg in project" :nvg "r" #'rg-project)
        (:prefix "s"
         :desc "Rg" :nvg "d" #'rg
         :desc "Rg Dwim" :nvg "D" #'rg-dwim
         :desc "Ripgrep Dispatch" :nvg "r" #'rg-menu
         :desc "List Searches" :nvg "R" #'rg-list-searches)))

;;;###package
(use-package! hardhat
  :defer t
  :init (setq hardhat-less-feedback t)
  :hook (doom-first-input . global-hardhat-mode))