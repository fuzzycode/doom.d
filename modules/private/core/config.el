;;; ~/.doom.d/modules/private/core/config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! use-package-chords)

;;;###package
(use-package! paradox
  :defer t
  :init (map! (:leader (:prefix "a"
                         :desc "Paradox List Packages" :g "l" 'paradox-list-packages)))
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
  :bind (([remap replace-regexp] . 'vr/replace)
         ([remap query-replace-regexp] . 'vr/query-replace)
         ("C-c r" . 'vr/replace)
         ("C-c q" . 'vr/query-replace)
         ("C-c m" . 'vr/mc-mark)))

;;;###package
(use-package! comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;;;###package
(use-package! smart-backspace
  :bind ([remap backward-delete-char-untabify] . smart-backspace))

;;;###package
(use-package! proced
   :defer t
   :hook (proced-mode . (lambda () (proced-toggle-auto-update +1)))
   :init (map! (:leader (:prefix ("a" . "applications")
                          :desc "Proced" :g "P" 'proced)))
   :config
   (setq proced-auto-update-interval 1)
   (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))

;;;###package
(use-package! mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)))

;;;###package
(use-package! winum
  :init (setq winum-auto-assign-0-to-minibuffer nil
              winum-auto-setup-mode-line nil
              winum-ignored-buffers '(" *which-key*"))
  :config (winum-mode)
  :bind (:map winum-keymap
          ("M-1" . 'winum-select-window-1)
          ("M-2" . 'winum-select-window-2)
          ("M-3" . 'winum-select-window-3)
          ("M-4" . 'winum-select-window-4)
          ("M-5" . 'winum-select-window-5)
          ("M-6" . 'winum-select-window-6)
          ("M-7" . 'winum-select-window-7)
          ("M-8" . 'winum-select-window-8)
          ("M-9" . 'winum-select-window-9)))

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
(use-package! deadgrep
  :defer t
  :init (map! (:leader
                (:prefix ("s" . "search")
                  :desc "Deadgrep" :g "d" 'deadgrep))))

;;;###package
(use-package! fill-column-indicator
  :defer t)

;;;###package
(use-package! expand-region
  :defer t
  :init (setq expand-region-contract-fast-key "V"
              expand-region-reset-fast-key "r")
  (map! (:leader
          :desc "Expand Region" :g "v" 'er/expand-region)))

;;;###package
(use-package! insert-shebang
  :defer t
  :init
  (map! (:leader (:prefix ("i" . "insert") :desc "Insert shebang" :g "!" 'insert-shebang)))
  (remove-hook 'find-file-hook 'insert-shebang))

;;;###package
(use-package! crux
  :defer t
  :commands crux-top-join-line
  :init
  (global-set-key (kbd "<A-up>") 'join-line)
  (global-set-key (kbd "<A-down>") 'crux-top-join-line)
  (map! (:leader
          (:prefix "b"
            :desc "Delete Buffer and File" :g "K" 'crux-delete-file-and-buffer
            :desc "Rename Buffer and File" :g "R" 'crux-rename-file-and-buffer)
          (:prefix ("f" . "file")
                   :desc "Find Shell init file" :g "S" 'crux-find-shell-init-file
                   :desc "Find User Custom File" :g "C" 'crux-find-user-custom-file)
          (:prefix ("x" . "text")
            :desc "Capitalize Region" :g "C" 'crux-capitalize-region
            (:prefix ("l" . "lines")
              :desc "Duplicate Line Or Region" :g "d" 'crux-duplicate-current-line-or-region
              :desc "Duplicate And Comment Line Or Region" :g "D" 'crux-duplicate-and-comment-current-line-or-region)))))

;;;###package
(use-package! dash-at-point
  :defer t
  :init
  (map! (:leader
          (:prefix ("d" . "documentation")
            :desc "Dash @ Point" :g "d" 'dash-at-point
            :desc "Dash @ Point With Docset" :g "D" 'dash-at-point-with-docset))))

;;;###package
(use-package! helm-dash
  :defer t
  :init (map! (:leader
               (:prefix ("d" . "documentation")
                 :desc "Helm Dash @ Point" :g "h" 'helm-dash-at-point
                 :desc "Helm Dash" :g "H" 'helm-dash))))

;;;###package
(use-package! lorem-ipsum
  :defer t
  :init (map! (:leader
                (:prefix ("i" . "insert")
                  (:prefix ("l" . "lorem/ipsum")
                    :desc "Paragraphs" :g "p" 'lorem-ipsum-insert-paragraphs
                    :desc "Sentences" :g "s" 'lorem-ipsum-insert-sentences
                    :desc "List" :g "l" 'lorem-ipsum-insert-list)))))

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
                (:prefix ("x" . "text")
                  (:prefix ("w" . "words")
                    :desc "Search Dictionary" :g "d" 'osx-dictionary-search-word-at-point)))))

;;;###package
(use-package! string-inflection
  :defer t
  :init (map! (:leader
                (:prefix ("x" . "text")
                  (:prefix ("i" . "inflection")
                    :desc "Cycle Style" :g [tab] '+core/inflection-cycle-dwim
                    :desc "Underscore" :g "u" 'string-inflection-underscore
                    :desc "Uppercase" :g "U" 'string-inflection-upcase
                    :desc "Kebab Case" :g "k" 'string-inflection-kebab-case
                    :desc "Lower Camel Case" :g "c" 'string-inflection-lower-camelcase
                    :desc "Camel Case" :g "C" 'string-inflection-camelcase)))))

;;;###package
(use-package! alert
  :defer t
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
  :commands (open-junk-file)
  :init (setq open-junk-file-format (concat doom-private-dir "junk/%Y/%m/%d-%H%M%S."))
  (map! (:leader
          (:prefix "f"
            :desc "Browse Junk Files" :g "J" #'+core/browse-junk-files
            :desc "Open Junk File" :g "j" #'+core/open-junk-file))))

;;;###package
(use-package! key-chord
  :defer 2
  :init
  (key-chord-define-global "uu" #'undo-tree-undo)
  (key-chord-define-global "kk" #'just-one-space)
  :config (shut-up (key-chord-mode +1)))

;;;###package
(use-package! avy
  :chords ("jj" . #'avy-goto-word-or-subword-1)
  :init (map! (:leader
                (:prefix ("j" . "jump/join")
                  :desc "Jump to Word" :g "j" #'avy-goto-word-or-subword-1
                  :desc "Jump to Line" :g "l" #'avy-goto-line))))

;;;###package
(use-package! goto-last-change
  :defer t
  :commands goto-last-change
  :init (map! (:leader (:prefix ("j" "jump/join")
                         :desc "Goto Last Change" :g "c" #'goto-last-change))))

;;;###package
(use-package! buffer-flip
  :chords (("u8" . buffer-flip))
  :bind  (:map buffer-flip-map
           ( "8" .   buffer-flip-forward)
           ( "*" .   buffer-flip-backward)
           ( "C-g" . buffer-flip-abort)))

;; EXTRAS
(load! "+bindings")
(load! "+popups")
