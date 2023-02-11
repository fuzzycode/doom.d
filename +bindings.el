;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;
;;; <leader>

(map! :leader
      ";" nil ;; Save for later
      "x" nil ;; No need for scratch buffer, use for text instead
      "w" nil
      "h" nil ;; I am used to my setup of help so I will use that
      ":" nil
      "<SPC>" nil

      ;; Remove deft keybinding if not using
      (:unless (modulep! :ui deft)
       (:prefix "n"
        "d" nil))

      ;; I do not use org clock feature so these can be removed
      (:prefix "n"
       "o" nil)

      (:when (modulep! :lang org)
       (:prefix "n"
        :desc "Org Roam Capture Today" "c" #'org-roam-dailies-capture-today
        :desc "Org Roam Capture" "C" #'org-roam-capture);; Override Doom binding
       (:prefix "o"
        :desc "Org Agenda" "A" (cmd! (org-agenda nil "a"))))

      (:when (modulep! :tools lsp)
       (:prefix "c"
        :desc "IMenu" "I" #'lsp-ui-imenu))

      (:prefix ("j" . "jump")) ;; Claim the j prefix for me
      (:prefix ("x" . "text")
       (:when (modulep! :ui hydra)
        :desc "Zoom" "z" #'+hydra/text-zoom/body)
       (:when (modulep! :editor rotate-text)
        :desc "Rotate text" "r" #'rotate-text))

      :desc "M-x" "<SPC>" #'execute-extended-command
      :desc "Eval Expression" ":" #'eval-expression
      :desc "Popup Scratch Buffer" "%" #'doom/open-scratch-buffer
      :desc "Shell Command" "!" #'shell-command
      :desc "Async Shell Command" "&" #'async-shell-command

      ;; Insert
      (:prefix "i"
       (:when (modulep! :editor evil)
        :desc "New Line Above" "k" #'+evil/insert-newline-above
        :desc "New Line Below" "j" #'+evil/insert-newline-below))

      ;; Git
      (:prefix "g"
       (:when (modulep! :ui hydra)
        :desc "Blame" "B" #'+bl/blame-hydra/body
        :desc "Git Time Machine" "t" #'+bl/timemachine-hydra/body)
       :desc "SMerge" "m" #'+bl/smerge-repeatedly
       :desc "Yank current Branch" "k" #'+bl/magit-add-current-branch-to-kill-ring)

      (:prefix "s"
       :desc "Fd Dired" "F" #'fd-dired)

      ;; Toggle
      (:prefix "t"
       :desc "Trailing Whitespace" :ng "w" (cmd! (setq show-trailing-whitespace (not show-trailing-whitespace)))
       :desc "Toggle Debug On Error" "t" #'toggle-debug-on-error
       (:after lsp-mode
        :desc "Breadcrumb Mode" :ng "h" #'lsp-headerline-breadcrumb-mode))

      ;; Notes
      (:prefix "n"
       :desc "Open Project Todo File" "p" (cmd! (find-file (+org-capture-project-todo-file)))
       :desc "Open Project Notes File" "P" (cmd! (find-file (+org-capture-project-notes-file)))
       :desc "Open Global Todo File" "x" (cmd! (find-file (+org-capture-todo-file)))
       :desc "Open Global Notes File" "X" (cmd! (find-file (+org-capture-notes-file)))
       :desc "Global Project Todo File" "g" (cmd! (find-file (+org-capture-central-project-todo-file)))
       :desc "Global Project Notes File" "G" (cmd! (find-file (+org-capture-central-project-notes-file))))

      ;; Open
      (:prefix "o"
       :desc "Dired Jump" "o" #'dired-jump
       :desc "Projectile Dired" "O" #'projectile-dired
       (:when (modulep! :emacs vc)
         :desc "Browse At Remote" "B" #'browse-at-remote)
       (:when (modulep! :tools prodigy)
        :desc "Prodigy" "y" #'prodigy)
       (:when (modulep! :app rss)
        :desc "Rss" "s" #'=rss))

      ;; Project
      (:prefix "p"
       "x" nil
       :desc "Scratch Buffer" "%" #'doom/open-project-scratch-buffer
       :desc "Capture Project Note" "n" (cmd! (org-capture nil "pn"))
       :desc "Open Project Notes" "N" (cmd! (find-file-existing (+org-capture-project-notes-file)))
       :desc "Capture Project Task" "t" (cmd! (org-capture nil "pp")))

      ;; Windows
      (:prefix "w"
       :desc "Balance Windows" "=" #'balance-windows
       :desc "Change Window" "w" #'ace-window
       :desc "Close Window" "k" #'+workspace/close-window-or-workspace
       :desc "Close Other Windows" "K" #'delete-other-windows
       :desc "Window Below" "b" #'split-window-below
       :desc "Window Below And Select" "B" (cmd! (select-window (split-window-below)))
       (:prefix ("m" . "maximize")
        :desc "Maximize Buffer" "m" #'doom/window-maximize-buffer
        :desc "Maximize Horizontally" "h" #'doom/window-maximize-horizontally
        :desc "Maximize Vertically" "v" #'doom/window-maximize-vertically)
       :desc "Enlarge" "o" #'doom/window-enlargen
       :desc "Most Recently Used" "p" #'evil-window-mru
       (:when (modulep! :ui treemacs)
         :desc "Project Sidebar" "s" #'treemacs-select-window)
       :desc "Other Window" "t" #'other-window
       :desc "Undo" "u" #'winner-undo
       :desc "Window Right" "r" #'split-window-right
       :desc "Window Right And Select" "R" (cmd! (select-window (split-window-right)))
       :desc "Quit" "q" #'evil-quit)

      ;; Help
      (:prefix "h"
       :desc "Info" "i" #'info
       :desc "Emacs News" "n" #'view-emacs-news
       :desc "Emacs Tutorial" "t" #'help-with-tutorial

       (:prefix ("a" . "apropos")
        :desc "Apropos" "a" #'apropos
        :desc "Command" "c" #'apropos-command
        :desc "Documentation" "d" #'apropos-documentation
        :desc "Documentation Property" "D" #'apropos-documentation-property
        :desc "Internal" "i" #'apropos-internal
        :desc "Library" "l" #'apropos-library
        :desc "Local Value" "V" #'apropos-local-value
        :desc "Local Variable" "E" #'apropos-local-variable
        :desc "Read Pattern" "r" #'apropos-read-pattern
        :desc "User Option" "o" #'apropos-user-option
        :desc "Value" "v" #'apropos-value
        :desc "Variable" "e" #'apropos-variable)
       (:prefix ("d" . "describe")
        :desc "Autodefs" "a" #'doom/describe-autodefs
        :desc "Char" "c" #'describe-char
        :desc "Bindings" "b" #'describe-bindings
        :desc "Doom Module" "d" #'doom/describe-module
        :desc "Function" "f" #'describe-function
        :desc "Face" "F" #'describe-face
        :desc "Key" "k" #'describe-key
        :desc "Language Environment" "L" #'describe-language-environment
        :desc "Mode" "m" #'describe-mode
        :desc "Active Mode" "M" #'doom/describe-active-minor-mode
        :desc "Symbol" "s" #'describe-symbol
        :desc "Package" "p" #'describe-package
        :desc "Doom Package" "P" #'doom/describe-package
        :desc "Theme" "t" #'describe-theme
        :desc "Variable" "v" #'describe-variable
        :desc "Text Properties" "T" #'describe-text-properties
        :desc "What cursor position" "w" #'what-cursor-position)
       (:prefix ("D" . "Doom")
        (:prefix ("b" . "Bump")
         :desc "Packages In Buffer" "b" #'doom/bump-packages-in-buffer
         :desc "Commit Bumps" "c" #'doom/commit-bumps
         :desc "Module" "m" #'doom/bump-module
         :desc "Package" "p" #'doom/bump-package
         :desc "Package At Point" "P" #'doom/bump-package-at-point)
        :desc "Info" "i" #'doom/info
        :desc "Issue Tracker" "I" #'doom/issue-tracker
        :desc "Homepage" "h" #'doom/homepage
        :desc "News" "n" #'doom/help-news
        :desc "Discourse" "d" #'doom/discourse
        :desc "Doom Manual" "D" #'doom/help
        :desc "Report Bug" "r" #'doom/report-bug
        :desc "Doom Reload" "R" #'doom/reload
        :desc "Version" "V" #'doom/version)))

(after! magit
  (map! :localleader
        :map with-editor-mode-map
        :desc "Commit" "c" #'with-editor-finish)
  (map! :localleader
        :map with-editor-mode-map
        :desc "Cancel" "k" #'with-editor-cancel))

(map! (:localleader
       :map emacs-lisp-mode-map
       (:prefix "d"
        :desc "Debug On Entry" "e" #'debug-on-entry
        :desc "Cancel Debug On Entry" "E" #'cancel-debug-on-entry
        :desc "Debug On Variable Change" "v" #'debug-on-variable-change
        :desc "Cancel Debug On Variable Change" "V" #'cancel-debug-on-variable-change)))

;; Bindings with no leader key
(map!
 (:when (not (modulep! :editor evil))
  "<A-up>" #'join-line)
 (:when (modulep! :editor evil)
  "<A-down>" #'evil-join)


 "C-x C-b" #'ibuffer
 "C-c l" #'recenter
 "C-c u" #'undo-fu-only-undo
 "C-u" #'undo-fu-only-undo
 "C-j" #'evil-scroll-down
 "C-k" #'evil-scroll-up

 :n "D" #'+lookup:dash
 :ng "M-." #'+lookup/definition
 :n "q" nil

 (:when (modulep! :editor evil) ;; Cleanup g prefix
  :nv "gi" nil
  :nv "g#" nil
  :nv "g$" nil
  :nv "g^" nil
  :nv "ge" nil
  :nv "gE" nil
  :nv "g8" nil
  :nv "gQ" nil
  :nv "g?" nil
  :nv "gr" #'+lookup/references
  :nv "ga" #'evil-avy-goto-char-timer
  :nv "gb" #'better-jumper-jump-backward

  :v "s" #'evil-surround-region

  (:when (modulep! :ui window-select +numbers)
   :n "g1" #'winum-select-window-1
   :n "g2" #'winum-select-window-2
   :n "g3" #'winum-select-window-3
   :n "g4" #'winum-select-window-4
   :n "g5" #'winum-select-window-5
   :n "g6" #'winum-select-window-6
   :n "g7" #'winum-select-window-7
   :n "g8" #'winum-select-window-8
   :n "g9" #'winum-select-window-9
   :n "g0" #'winum-select-window-0-or-10))
 (:after magit
  (:map magit-status-mode-map
   :n "yu" #'+bl/kill-url-to-commit-at-point
   :n "C-j" #'magit-section-forward-sibling
   :n "C-k" #'magit-section-backward-sibling)
  (:map magit-revision-mode-map
   :n "yu" #'+bl/kill-url-to-commit-at-point)
  (:map magit-log-mode-map
   :n "yu" #'+bl/kill-url-to-commit-at-point))
 (:after (forge code-review)
  (:map magit-status-mode-map
   "C-c C-r" #'+magit/start-code-review)
  (:map forge-pullreq-list-mode-map
   "C-c C-r" #'+magit/start-code-review)
  (:map forge-topic-mode-map
   "r" #'+magit/start-code-review)
  (:map code-review-feedback-section-map
   "x" #'code-review-delete-feedback)
  (:map code-review-local-comment-section-map
   "x" #'code-review-section-delete-comment)
  (:map code-review-reply-comment-section-map
   "x" #'code-review-section-delete-comment)
  (:map code-review-mode-map
   :n "gr" #'code-review-reload))
 (:after flyspell
  (:map flyspell-mode-map
   :ngi "M-i" #'flyspell-correct-wrapper))
 (:after (projectile cc-mode)
  (:map c++-mode-map
   :n "go" #'+bl/toggle-header-source
   :ngi "<A-tab>" #'+bl/toggle-header-source))
 (:after projectile
  :ngi "M-o" #'projectile-find-file-dwim)
 (:after smartparens
  (:map smartparens-mode-map
   :ngi "C-<right>" #'sp-forward-slurp-sexp
   :ngi "C-<left>" #'sp-forward-barf-sexp
   :ngi "C-M-<right>" #'sp-backward-slurp-sexp
   :ngi "C-M-<left>" #'sp-backward-barf-sexp))
 (:after lsp-mode
  (:map lsp-mode-map
   :ngi "<A-return>" #'lsp-execute-code-action))
 (:map lsp-ui-peek-mode-map
  "<tab>" #'lsp-ui-peek--toggle-file)
 (:after ranger
  (:map ranger-mode-map
   [escape] #'ranger-close))
 (:after ibuffer
  (:map ibuffer-mode-map
   :n [escape] #'kill-current-buffer))
 (:after org-agenda
  (:map org-agenda-mode-map
   [escape] #'+popup/quit-window
   :ng "q" #'+popup/quit-window))
 (:after tabulated-list
  (:map tabulated-list-mode-map
   :ng "q" #'quit-window)))

;; (after! which-key
;;   (pushnew! which-key-replacement-alist
;;             '((nil . "\\+?evil\\(?:nc\\|em\\)?[:/-]\\(?:a-\\|motion-\\)?\\(.+\\)") . (nil . +bl/beautify-evil))
;;             '((nil . "\\(?:\\?\\|consult-\\)?org[:/-]\\(.+\\)") . (nil . +bl/beautify-org))
;;             '((nil . "\\+\\(.+\\)[:/]\\(.+\\)") . (nil . +bl/beautify-doom))
;;             '((nil . ".+-.+") . (nil . +bl/format-command-name))))
