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
      "y" nil

      (:prefix ("y" . "Yank"))
      (:prefix ("l" . "LLMs"))

      ;; Remove deft keybinding if not using
      (:unless (modulep! :ui deft)
        (:prefix "n"
                 "d" nil))

      ;; I do not use org clock feature so these can be removed
      (:prefix "n"
               "o" nil)

      (:when (modulep! :tools lsp)
        (:prefix "c"
         :desc "IMenu" "I" #'lsp-ui-imenu))

      (:prefix ("j" . "jump")) ;; Claim the j prefix for me
      (:prefix ("x" . "text")
               (:prefix ("n" . "narrow")
                :nv "r" #'narrow-to-region
                :nv "d" #'narrow-to-defun
                :nv "p" #'narrow-to-page
                :nv "w" #'widen)
               :desc "Scale" "z" #'text-zoom-transient
               (:when (modulep! :editor rotate-text)
                 :desc "Rotate text" "r" #'rotate-text))

      :desc "M-x" "<SPC>" #'execute-extended-command
      :desc "Eval Expression" ":" #'eval-expression
      :desc "Popup Scratch Buffer" "%" #'doom/open-scratch-buffer
      :desc "Shell Command" "!" #'shell-command
      :desc "Async Shell Command" "&" #'async-shell-command

      (:prefix "b"
       :desc "Switch to Message Buffer" "L" #'+bl/switch-to-message-buffer)

      ;; Insert
      (:prefix "i"
               (:when (modulep! :editor evil)
                 :desc "New Line Above" "k" #'+evil/insert-newline-above
                 :desc "New Line Below" "j" #'+evil/insert-newline-below))

      ;; Jump
      (:prefix "j"
       :desc "Goto Char Timer" "j" #'avy-goto-char-timer
       :desc "Find Other File" "o" #'projectile-find-other-file
       :desc "Find Other File Other Window" "O" #'projectile-find-other-file-other-window
       :desc "Ace Window" "w" #'ace-window
       (:after smartparens
        :desc "Forward S-exp" "f" #'sp-forward-sexp
        :desc "Backward S-exp" "b" #'sp-backward-sexp))

      ;; Git
      (:prefix "g"
       (:when (modulep! :tools magit +forge)
         (:prefix "p"
          :desc "Create" "c" #'forge-create-pullreq
          :desc "List (forge)" "l" #'forge-list-pullreqs))

       :desc "SMerge" "m" #'+bl/smerge-repeatedly
       :desc "Update" "u" #'+bl/git-repo-sync
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
       "e" nil
       :desc "Org Agenda List" "a" #'org-agenda-list
       :desc "Org Agenda" "A" #'org-agenda
       :desc "Org Roam Capture" "c" #'org-roam-capture
       :desc "Find Node" "f" #'org-roam-node-find
       :desc "Capture Snippet" "C" #'+bl/org-roam-capture-snippet
       :desc "Capture Daily" "d" #'org-roam-dailies-capture-today
       :desc "Capture Inbox" "i" #'+bl/org-roam-capture-inbox
       :desc "Open Inbox" "I" #'+bl/org-roam-open-inbox
       :desc "Org Roam Find Node" "n" #'+bl/org-roam-capture-default
       :desc "Org Capture" "N" #'org-capture
       :desc "Find Project" "p" #'+bl/org-roam-find-project
       :desc "Add Project Task" "P" #'+bl/org-roam-capture-project-task
       :desc "Org Ql Search" "q" #'org-ql-search)

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
       :desc "Find Project Notes" "n" #'+bl/org-roam-find-project
       :desc "Capture Project Task" "t" #'+bl/org-roam-capture-project-task)

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
      (:prefix "x"
       :desc "Capitalize" "c" #'capitalize-dwim
       :desc "Downcase" "d" #'downcase-dwim
       :desc "Upcase" "u" #'upcase-dwim)
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
        :desc "Key Map" "K" #'describe-keymap
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


 "<mouse-4>" #'previous-buffer
 "<mouse-5>" #'next-buffer

 "C-x C-b" #'ibuffer
 "C-c l" #'recenter
 "C-c u" #'undo-fu-only-undo
 "C-u" #'undo-fu-only-undo
 "C-j" #'evil-scroll-down
 "C-k" #'evil-scroll-up
 "M-g" #'avy-goto-line
 "M-g M-g" #'avy-goto-line

 :n "D" #'+lookup:dash
 (:when (modulep! :completion vertico)
   :ng "M-." #'embark-dwim)
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
   :v "v" #'er/expand-region
   :v "V" #'er/contract-region

   :i "C-y" #'evil-paste-after
   :i "C-Y" #'evil-paste-before


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
          :n "X" #'magit-reset
          :n "o" #'magit-submodule
          :n "O" #'magit-subtree
          "C-j" #'magit-section-forward-sibling
          "C-k" #'magit-section-backward-sibling))
 (:after (forge pr-review)
         (:map magit-status-mode-map
               "C-c C-r" #'+bl/pr-review-from-forge-maybe))
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
          :n "gF" #'sp-forward-sexp
          :n "gB" #'sp-backward-sexp
          :ngi "C-<right>" #'sp-forward-slurp-sexp
          :ngi "C-<left>" #'sp-forward-barf-sexp
          :ngi "C-M-<right>" #'sp-backward-slurp-sexp
          :ngi "C-M-<left>" #'sp-backward-barf-sexp))
 (:after org-mode
         (:map org-mode-map
               "C-M-i" #'completion-at-point))
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
 (:after org-roam
         (:map org-mode-map
          :ngvi "M-n" #'+bl/org-roam-node-insert-immediate))
 (:after tabulated-list
         (:map tabulated-list-mode-map
          :ng "q" #'quit-window)))

(after! which-key
  (pushnew! which-key-replacement-alist
            '((nil . "\\+?evil\\(?:nc\\)?[-:/]\\(?:a\\|inner\\|outer\\)?\\(.+\\)") . (nil . +bl/beautify-evil))
            ;; TODO: Make this skip my +bl/ functions
            ;; '((nil . "\\+\\(.+\\)[:/]\\(.+\\)") . (nil . +bl/beautify-doom))
            ))

;; (after! which-key (pushnew! which-key-replacement-alist
;;             '((nil . "\\+?evil\\(?:nc\\|em\\)?[:/-]\\(?:a-\\|motion-\\)?\\(.+\\)") . (nil . +bl/beautify-evil))
;;             '((nil . "\\(?:\\?\\|consult-\\)?org[:/-]\\(.+\\)") . (nil . +bl/beautify-org))
;;             '((nil . "\\+\\(.+\\)[:/]\\(.+\\)") . (nil . +bl/beautify-doom))
;;             '((nil . ".+-.+") . (nil . +bl/format-command-name))))
