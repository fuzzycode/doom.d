;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
       (:prefix "h"
          :desc "Info" :ng "i" #'info
          (:prefix ("d" . "describe")
            :desc "Char" :ng "c" #'describe-char
            :desc "Bindings" :ng "b" #'describe-bindings
            :desc "Function" :ng "f" #'describe-function
            :desc "Face" :ng "F" #'describe-face
            :desc "Key" :ng "k" #'describe-key
            :desc "Mode" :ng "m" #'describe-mode
            :desc "Symbol" :ng "s" #'describe-symbol
            :desc "Package" :ng "p" #'describe-package
            :desc "Theme" :ng "t" #'describe-theme
            :desc "Variable" :ng "v" #'describe-variable
            :desc "Text Properties" :ng "T" #'describe-text-properties)
          (:prefix ("l" . "lookup")
           :desc "Documentation" :ng "d" #'+lookup:dash
           :desc "Online" :ng "o" #'+lookup:online))))

(map! (:leader
        (:prefix "s"
          (:prefix ("m" . "multiple cursors")
            :desc "Mark All" :ng "a" #'mc/mark-all-dwim
            :desc "Mark All Like This" :ng "b" #'mc/mark-all-like-this
            :desc "Mark More Like This" :ng "m" #'mc/mark-more-like-this-extended
            :desc "Edit Lines" :ng "r" #'mc/edit-lines
            (:prefix ("s" . "insert/sort")
              :desc "Inert Letters" :ng "l" #'mc/insert-letters
              :desc "Mark SGML pair" :ng "m" #'mc/mark-sgml-tag-pair
              :desc "Insert Numbers" :ng "n" #'mc/insert-numbers
              :desc "Sort Regions" :ng "s" #'mc/sort-regions
              :desc "Reverse Regions" :ng "t" #'mc/reverse-regions)))))

(map! (:leader (:prefix "x"
                 :desc "Zoom Text" :ng "z" #'+hydra/text-zoom/body)))

(map! (:leader (:prefix "x"
                 (:prefix ("f" . "folding")
                   :when (featurep! :editor fold)
                   :desc "Close All" :ng "C" #'+fold/close-all
                   :desc "Open All" :ng "O" #'+fold/open-all
                   :desc "Close" :ng "c" #'+fold/close
                   :desc "Open" :ng "o" #'+fold/open
                   :desc "Toggle" :ng "t" #'+fold/toggle
                   :desc "Next" :ng "n" #'+fold/next
                   :desc "Previous" :ng "p" #'+fold/previous))))

(map! (:leader (:prefix "n"
                 :desc "Narrow To Region" :ng "r" #'narrow-to-region
                 :desc "Narrow To Defun" :ng "d" #'narrow-to-defun
                 :desc "Narrow To Page" :ng "p" #'narrow-to-page
                 :desc "Widen" :ng "w" #'widen)))

(map! (:leader (:prefix "k"
        :desc "Barf Forward" :ng "b" #'sp-forward-barf-sexp
        :desc "Barf Backward" :ng "B" #'sp-backward-barf-sexp
        :desc "Slurp Forward" :ng "s" #'sp-forward-slurp-sexp
        :desc "Slurp Backward" :ng "S" #'sp-backward-slurp-sexp
        :desc "Absorb Sexp" :ng "a" #'sp-absorb-sexp
        :desc "Convolute Sexp" :ng "c" #'sp-convolute-sexp
        :desc "Previous Sexp" :ng "p" #'sp-previous-sexp
        :desc "Next Sexp" :ng "n" #'sp-next-sexp
        :desc "Transpose Sexp" :ng "t" #'sp-transpose-sexp
        :desc "Split Sexp" :ng "i" #'sp-split-sexp
        :desc "Splice Sexp" :ng "I" #'sp-splice-sexp)))

(map! (:leader (:prefix "f"
                :desc "Find User Init File" :ng "i" #'doom/goto-private-init-file)))

(map! (:leader
        :desc "Universal Argument" :ng "u" #'universal-argument
        :desc "Undo" :ng "," #'undo-fu-only-undo
        :desc "Repeat" :ng "." #'evil-repeat-pop-next
        (:prefix "a"
         :desc "Dired" :ng "d" #'dired
         (:when (featurep! :email mu4e)
          (:prefix ("m" . "mail")
           :desc "Compose" :ng "c" #'+mu4e/compose
           :desc "Mail" :ng "m" #'mu4e))
          (:prefix ("s" . "shell")
            (:when (featurep! :term vterm)
              :desc "Toggle vterm popup"    "v" #'+vterm/toggle
              :desc "Open vterm here"       "V" #'+vterm/here)
            (:when (featurep! :term eshell)
              :desc "Toggle eshell popup"   "e" #'+eshell/toggle
              :desc "Open eshell here"      "E" #'+eshell/here)))
        (:prefix "x"
          :desc "Count Region" :ng "c" #'count-words-region
          :desc "Indent Rigidly" :ng "TAB" #'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :ng "c" #'transpose-chars
            :desc "Lines" :ng "l" #'transpose-lines
            :desc "Words" :ng "w" #'transpose-words))
        (:prefix "s"
          :desc "List Links" :ng "L" #'ffap-menu)
        (:prefix "l"
         :desc "Uniquify Lines" :ng "u" #'+bl/uniquify-lines-dwim
         :desc "Center Line" :ng "c" #'recenter-top-bottom)
        (:prefix "b"
          :desc "Find file in emacs.d" :ng "e"  #'doom/find-file-in-emacsd
          :desc "Browse emacs.d" :ng "E"  #'doom/browse-in-emacsd
          :desc "Save" :ng "s" #'save-buffer
          :desc "Scratch Buffer" :ng "S" #'+bl/switch-to-scratch-buffer
          :desc "Messages Buffer" :ng "M" #'+bl/switch-to-message-buffer
          :desc "New Buffer" :ng "N" #'+bl/new-empty-buffer
          :desc "Buffer to Clipboard" :ng "P" #'+bl/copy-whole-buffer-to-clipboard
          :desc "Clipboard to Buffer" :ng "Y" #'+bl/copy-clipboard-to-whole-buffer
          :desc "Workspace Buffer" :ng "w" #'+ivy/switch-workspace-buffer
          :desc "Read Only" :ng "W" #'read-only-mode
          :desc "Next Buffer" :ng "n" #'next-buffer
          :desc "Previous Buffer" :ng "p" #'previous-buffer
          :desc "Doom Dashboard" :ng "d" #'+doom-dashboard/open
          :desc "Doom Sandbox" :ng "D" #'doom/sandbox
          :desc "Ibuffer" :ng "I" #'ibuffer
          :desc "Sudo find file" :ng "u" #'doom/sudo-find-file
          :desc "Sudo this file" :ng "U" #'doom/sudo-this-file
          :desc "Kill Buffer and Window" :ng "x" #'kill-buffer-and-window
          :desc "Kill Matching Buffers" :ng "C-d" #'kill-matching-buffers
          :desc "Show and Copy Buffer Filename" :ng "C" #'+bl/show-and-copy-buffer-filename)
        (:prefix "i"
          :desc "New Line" :ng "n" #'sp-newline
          :desc "Open Line" :ng "o" #'open-line
          :desc "Insert Snippet" :ng "s" #'yas-insert-snippet
          :desc "Insert Buffer" :ng "b" #'insert-buffer)
        (:prefix "j"
          :desc "Deer" :ng "d" #'deer)
        (:prefix "f"
         :desc "Move This File" :ng "m" #'doom/move-this-file
          (:prefix ("D" . "doom")
            :desc "Packages File" :ng "p" #'doom/goto-packages-file
            :desc "Config File" :ng "C" #'doom/goto-config-file
            :desc "Init File" :ng "i" #'doom/goto-doomblock
            :desc "User Config" :ng "c" #'doom/open-private-config))
        (:prefix "h"
          :desc "Version" :ng "V" #'doom/version
          (:prefix ("t" . "tutorials")
            :desc "Emacs Tutorial" :ng "e" #'help-with-tutorial)
          (:prefix "d"
           :desc "DOOM Autodefs" :ng "A" #'doom/describe-autodefs
           :desc "Language Environment" :ng "L" #'describe-language-environment
           :desc "Minor Mode(s)" :ng "M" #'doom/describe-active-minor-mode
           :desc "DOOM Packages" :ng "P" #'doom/help-packages
           :desc "DOOM Modules" :ng "D" #'doom/help-modules))
        (:prefix "j"
          (:when (featurep! :ui window-select)
            :desc "Jump to Window" :ng "w" #'ace-window))
        (:prefix "w"
          :desc "Save Session" :ng "q" #'doom/quicksave-session
          :desc "Load Session" :ng "Q" #'doom/quickload-session
          :desc "Make Frame" :ng "F" #'make-frame
          :desc "Other Frame" :ng "o" #'other-frame
          :desc "Winner Redo" :ng "U" #'winner-redo
          :desc "Winner Undo" :ng "u" #'winner-undo
          :desc "Split Window Right" :ng "v" #'split-window-right
          :desc "Split Window Right & Focus" :ng "V" #'split-window-right-and-focus
          :desc "Split Window Below" :ng "s" #'split-window-below
          :desc "Split Window Below & Focus" :ng "S" #'split-window-below-and-focus
          :desc "Balance Windows" :ng "=" #'balance-windows
          :desc "Close Window" :ng "k" #'+workspace/close-window-or-workspace
          :desc "Delete Other Windows" :ng "K" #'delete-other-windows
          (:when (featurep! :ui window-select)
            :desc "Ace Window" :ng "w" #'ace-window))
        (:prefix "K"
          :desc "Start or Insert Counter" :ng "r" #'kmacro-start-macro-or-insert-counter
          :desc "End or Call Macro" :ng "f" #'kmacro-end-or-call-macro-repeat
          :desc "Add Counter" :ng "a" #'kmacro-add-counter
          :desc "Bind to Key" :ng "b" #'kmacro-bind-to-key
          :desc "Call Macro" :ng "c" #'kmacro-call-macro
          :desc "Delete Ring Head" :ng "d" #'kmacro-delete-ring-head
          :desc "Insert Counter" :ng "i" #'kmacro-insert-counter
          :desc "End Call Mouse" :ng "m" #'kmacro-end-call-mouse
          :desc "Cycle Ring Next" :ng "n" #'kmacro-cycle-ring-next
          :desc "Name Last Macro" :ng "N" #'kmacro-name-last-macro
          :desc "Cycle Ring Previous" :ng "p" #'kmacro-cycle-ring-previous
          :desc "View Macro" :ng "v" #'kmacro-view-macro
          :desc "View Macro Repeat" :ng "V" #'kmacro-view-macro-repeat
          :desc "Macro to Register" :ng "w" #'kmacro-to-register
          :desc "Jump to Register" :ng "y" #'jump-to-register
          (:prefix ("2" . "2nd")
            :desc "Call Ring 2nd" :ng "c" #'kmacro-call-ring-2nd
            :desc "Call Ring 2nd Repeat" :ng "C" #'kmacro-call-ring-2nd-repeat
            :desc "View Ring 2nd" :ng "v" #'kmacro-view-ring-2nd)
          (:prefix ("e" . "edit")
            :desc "Edit Lossage" :ng "l" #'kmacro-edit-lossage
            :desc "Edit Macro" :ng "m" #'kmacro-edit-macro
            :desc "Edit Macro Repeat" :ng "r" #'kmacro-edit-macro-repeat
            :desc "Step Edit Macro" :ng "t" #'kmacro-step-edit-macro)
          (:prefix ("s" . "set")
            :desc "Set Counter" :ng "c" #'kmacro-set-counter
            :desc "Set Format" :ng "f" #'kmacro-set-format
            :desc "Swap Ring" :ng "r" #'kmacro-swap-ring))
        (:prefix "R"
          :desc "Clear Rectangle" :ng "!" #'clear-rectangle
          :desc "Close Rectangle" :ng "c" #'close-rectangle
          :desc "Delete Rectangle" :ng "d" #'delete-rectangle
          :desc "Exchange Point & Mark" :ng "e" #'rectangle-exchange-point-and-mark
          :desc "Copy To Register" :ng "i" #'copy-rectangle-to-register
          :desc "Kill Rectangle" :ng "k" #'kill-rectangle
          :desc "Left Char" :ng "l" #'rectangle-left-char
          :desc "Mark Mode" :ng "m" #'rectangle-mark-mode
          :desc "Next Line" :ng "n" #'rectangle-next-line
          :desc "Number Lines" :ng "N" #'rectangle-number-lines
          :desc "Open Rectangle" :ng "o" #'open-rectangle
          :desc "Previous Line" :ng "p" #'rectangle-previous-line
          :desc "Right Char" :ng "r" #'rectangle-right-char
          :desc "String Rectangle" :ng "s" #'string-rectangle
          :desc "Transpose Regions" :ng "t" #'transpose-regions
          :desc "Yank Rectangle" :ng "y" #'yank-rectangle)
        (:prefix "E"
          (:prefix ("b" . "buffers")
            :desc "Buffers 3 Way" :ng "3" #'ediff-buffers3
            :desc "Buffers" :ng "b" #'ediff-buffers
            :desc "Patch Buffer" :ng "p" #'ediff-patch-buffer)
          (:prefix ("d" . "directories")
            :desc "Directories 3 Way" :ng "3" #'ediff-directories3
            :desc "Directories" :ng "d" #'ediff-directories
            :desc "Directory Revisions" :ng "r" #'ediff-directory-revisions)
          (:prefix ("f" . "files")
            :desc "File 3 Way" :ng "3" #'ediff-files3
            :desc "Files" :ng "f" #'ediff-files
            :desc "Patch File" :ng "p" #'ediff-patch-file)
          (:prefix ("m" . "merge")
            (:prefix ("b" . "buffers")
              :desc "Merge Buffers with Ancestor" :ng "3" #'ediff-merge-buffers-with-ancestor
              :desc "Merge Buffers" :ng "b" #'ediff-merge-buffers)
            (:prefix ("d" . "directories")
              :desc "Merge Directories With Ancestor" :ng "3" #'ediff-merge-directories-with-ancestor
              :desc "Merge Directories" :ng "d" #'ediff-merge-directories)
            (:prefix ("f" . "files")
              :desc "Merge Files With Ancestor" :ng "3" #'ediff-merge-files-with-ancestor
              :desc "Merge Files" :ng "m" #'ediff-merge-files)
            (:prefix ("r" . "revisions")
              :desc "Merge Revisions With Ancestor" :ng "3" #'ediff-merge-revisions-with-ancestor
              :desc "Merge Revisions" :ng "r" #'ediff-merge-revisions))
          (:prefix ("r" . "regions/revisions")
            :desc "Diff Regions Linewise" :ng "l" #'ediff-regions-linewise
            :desc "Revisions" :ng "r" #'ediff-revision
            :desc "Diff Regions Wordwise" :ng "w" #'ediff-regions-wordwise)
          (:prefix ("w" . "windows")
            :desc "Diff Windows Linewise" :ng "l" #'ediff-windows-linewise
            :desc "Diff Windows Wordwise" :ng "w" #'ediff-windows-wordwise)
          :desc "Backup" :ng "B" #'ediff-backup
          :desc "Documentation" :ng "h" #'ediff-documentation
          :desc "Show Registry" :ng "s" #'ediff-show-registry)))

(map! (:leader
        :desc "Shell Command" :ng "!" 'shell-command
        :desc "Eval" :ng ":" 'eval-expression
        (:when (featurep! :tools ein)
          (:prefix "a"
            (:prefix ("y" . "jupyter notebooks")
              :desc "Login" :ng "l" #'ein:notebooklist-login
              :desc "Open" :ng "o" #'ein:notebooklist-open
              :desc "Run" :ng "r" #'ein:run
              :desc "Stop" :ng "s" #'ein:stop)))
        (:prefix "e"
          (:when (featurep! :checkers syntax)
            :desc "Next Error" :ng "n" #'flycheck-next-error
            :desc "Previous Error" :ng "p" #'flycheck-previous-error
            :desc "List Errors" :ng "l" #'flycheck-list-errors
            :desc "Verify Setup" :ng "v" #'flycheck-verify-setup))
        (:prefix "x"
          :desc "Downcase Region" :ng "d" 'downcase-region
          :desc "Upcase Region" :ng "u" 'upcase-region
          (:prefix ("i" . "indent")
            :desc "Indent Buffer" :ng "b" #'+text/indent-buffer
            :desc "Indent Defun" :ng "d" #'sp-indent-defun
            :desc "Indent Region" :ng "r" #'indent-region
            :desc "Indent Region Or Buffer" :ng "i" #'+text/indent-region-or-buffer))
        (:prefix "h"
          :desc "Emacs News" :ng "n" #'view-emacs-news
          :desc "Doom Manual" :ng "D" #'doom/help)
        (:prefix "q"
          :desc "Quit Emacs"                  :ng "q" #'kill-emacs
          :desc "Save and quit Emacs"         :ng "Q" #'save-buffers-kill-terminal
          :desc "Quick save current session"  :ng "s" #'doom/quicksave-session
          :desc "Restore last session"        :ng "l" #'doom/quickload-session
          :desc "Restart emacs server"        :ng "d" #'+default/restart-server
          :desc "Delete frame"                :ng "f" #'delete-frame
          :desc "Clear current frame"         :ng "F" #'doom/kill-all-buffers
          :desc "Kill Emacs (and daemon)"     :ng "K" #'save-buffers-kill-emacs
          (:when (featurep! :ui workspaces)
            :desc "Quit Emacs & forget session" :ng "X" #'+workspace/kill-session-and-quit)
          :desc "Restart & restore Emacs"      :ng "r" #'doom/restart-and-restore
          :desc "Restart Emacs"                :ng "R" #'doom/restart)))

(map! (:localleader
        :map json-mode-map
        (:prefix ("=" . "format")
          :desc "Format Region Or Buffer" :ng "=" #'+json/pretty-print-region-or-buffer
          :desc "Format Buffer" :ng "b" #'json-pretty-print-buffer
          :desc "Format Region" :ng "r" #'json-pretty-print)))

(map! :localleader
        :map emacs-lisp-mode-map
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
          :desc "Instrument Defun ON" "f" #'+emacs-lisp/edebug-instrument-defun-on
          :desc "Instrument Defun OFF" "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
          :desc "Eval Buffer" "b" #'eval-buffer
          :desc "Eval Defun" "d" #'eval-defun
          :desc "Eval Last s-exp" "e" #'eval-last-sexp
          :desc "Eval Region" "r" #'eval-region
          :desc "Load Library" "l" #'load-library)
        (:prefix ("g" . "goto")
          :desc "Find Function" "f" #'find-function
          :desc "Find Variable" "v" #'find-variable
          :desc "Find Library" "l" #'find-library))

(map! (:after projectile
       :n "go" #'projectile-find-other-file
       (:leader
          (:prefix "p"
            :desc "Shell Command" :ng "!" #'projectile-run-shell-command-in-root
            :desc "Async Shell Command" :ng "&" #'projectile-run-async-shell-command-in-root
            :desc "Edit dir-locals" :ng "e" #'projectile-edit-dir-locals
            :desc "Compile" :ng "c" #'projectile-compile-project
            :desc "Configure" :ng "C" #'projectile-configure-project
            :desc "Dired" :ng "d" #'projectile-dired
            :desc "Kill Buffers" :ng "k" #'projectile-kill-buffers
            :desc "Test Project" :ng "t" #'projectile-test-project
            :desc "Shell" :ng "s" #'projectile-run-vterm
            :desc "Save Project Files" :ng "S" #'projectile-save-project-buffers
            :desc "Find file in other project" :ng "O" #'doom/find-file-in-other-project
            :desc "Project Scratch Buffer" :ng "x" #'doom/open-project-scratch-buffer))))

(map! (:after persp-mode
        (:leader
          (:prefix "p"
            :desc "Switch Perspective" :ng "P" #'persp-switch))))

(map! (:leader
       (:prefix "c"
        :desc "Comment/Uncomment" :ng "c" #'comment-dwim-2
        :desc "Find Definition" :ng "d" #'+lookup/definition
        :desc "Find References" :ng "r" #'+lookup/references
        :desc "Find Implementations" :ng "i" #'+lookup/implementations
        :desc "Find Type Definition" :ng "t" #'+lookup/type-definition
        :desc "Evaluate buffer/region" :ng "e" #'+eval/buffer-or-region
        :desc "Evaluate & replace region" :ng "E" #'+eval:replace-region
        :desc "Format buffer/region" :ng "f" #'+format/region-or-buffer
        :desc "Lookup Docset" :ng "h" #'+lookup/in-docsets
        :desc "Send to repl" :ng "s"  #'+eval/send-region-to-repl
        :desc "Delete trailing white space" :ng "w" #'delete-trailing-whitespace
        :desc "Delete trailing newlines" :ng "W" #'doom/delete-trailing-newlines
        (:when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
         :desc "Execute code action" :ng "a" #'lsp-execute-code-action
         :desc "Organize imports" :ng "o" #'lsp-organize-imports
         (:when (featurep! :completion ivy)
         :desc "Jump to symbol in current workspace" :ng "j"   #'lsp-ivy-workspace-symbol
         :desc "Jump to symbol in any workspace" :ng "J"   #'lsp-ivy-global-workspace-symbol))
        (:when (featurep! :checkers syntax)
         :desc "List errors" :ng "x" #'flycheck-list-errors))))

(map! (:leader
       (:when (featurep! :app rss)
        (:prefix "a"
         :desc "News Feed" :ng "n" #'elfeed)
        (:prefix "f"
         (:prefix "o"
          :desc "Open Elfeed File(s)" :ng "e" #'+org/open-efeed-files)))))

(map! (:after evil
       (:leader
       (:prefix "j"
        :desc "Beginning of line" :nvg "a" #'evil-beginning-of-line
        :desc "End of line" :nvg "e" #'evil-end-of-line))))

(map! (:after flyspell
        (:leader
         (:prefix "S"
          :desc "Add Word" :ng "a" #'+spell/add-word
          :desc "Remove Word" :ng "r" #'+spell/remove-word
          :desc "Next Error" :ng "j" #'+spell/next-error
          :desc "Previous Error" :ng "k" #'+spell/previous-error
          :desc "Correct Next" :ng "n" #'flyspell-correct-next
          :desc "Correct Previous" :ng "p" #'flyspell-correct-previous
          :desc "Correct At Point" :ng "c" #'flyspell-correct-at-point
          :desc "Correct DWIM" :ng "s" #'flyspell-correct-wrapper
          :desc "Change Dictionary" :ng "d" #'ispell-change-dictionary))))

(map! (:leader
       (:prefix "t"
        :desc "Word Wrap" :ng "w" #'+word-wrap-mode
        :desc "Big Font" :ng "b" #'doom-big-font-mode
        :desc "Flycheck" :ng "f" #'flycheck-mode
        :desc "Read Only" :ng "r" #'read-only-mode
        :desc "Trailing Whitespace" :ng "w" (cmd! (setq show-trailing-whitespace (not show-trailing-whitespace)))
        :desc "Relative Line Numbers" :ng "l" #'toggle-relative-line-numbers
        (:after lsp-mode
         :desc "Breadcrumb Mode" :ng "h" #'lsp-headerline-breadcrumb-mode )
        (:after smartparens
         :desc "Smartparens Strict Mode" :ng "s" #'smartparens-strict-mode))))

(map! :ng "M-." #'+lookup/definition
      :ng "C-x C-b" #'ibuffer
      :ng "C-c l" #'recenter

      (:after flyspell
       (:map flyspell-mode-map
        :ng "M-i" #'flyspell-correct-wrapper))
      (:after (projectile cc-mode)
       (:map c++-mode-map
        :ng "<A-tab>" #'projectile-find-other-file))

      :n "q" nil
      :n "J" #'+lookup:dash
      :n "C-j" #'evil-scroll-down
      :n "C-k" #'evil-scroll-up)

;; Remove binding, I did not need it and it was colliding with org mode keys
(after! pyenv-mode
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

;;; Make q close the window, not just the buffer
(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook (lambda ()
                                        (define-key xwidget-webkit-mode-map (kbd "<up>") #'xwidget-webkit-scroll-up-line)
                                        (define-key xwidget-webkit-mode-map "<down>" #'xwidget-webkit-scroll-down-line)
                                        (define-key xwidget-webkit-mode-map  "q" #'+workspace/close-window-or-workspace))))
