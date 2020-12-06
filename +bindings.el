;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
       (:prefix "h"
          :desc "Info" :nvg "i" #'info
          (:prefix ("d" . "describe")
            :desc "Char" :nvg "c" #'describe-char
            :desc "Bindings" :nvg "b" #'describe-bindings
            :desc "Function" :nvg "f" #'describe-function
            :desc "Face" :nvg "F" #'describe-face
            :desc "Key" :nvg "k" #'describe-key
            :desc "Mode" :nvg "m" #'describe-mode
            :desc "Symbol" :nvg "s" #'describe-symbol
            :desc "Package" :nvg "p" #'describe-package
            :desc "Theme" :nvg "t" #'describe-theme
            :desc "Variable" :nvg "v" #'describe-variable
            :desc "Text Properties" :nvg "T" #'describe-text-properties))))

(map! (:leader
        (:prefix "s"
          (:prefix ("m" . "multiple cursors")
            :desc "Mark All" :nvg "a" #'mc/mark-all-dwim
            :desc "Mark All Like This" :nvg "b" #'mc/mark-all-like-this
            :desc "Mark More Like This" :nvg "m" #'mc/mark-more-like-this-extended
            :desc "Edit Lines" :nvg "r" #'mc/edit-lines
            (:prefix ("s" . "insert/sort")
              :desc "Inert Letters" :nvg "l" #'mc/insert-letters
              :desc "Mark SGML pair" :nvg "m" #'mc/mark-sgml-tag-pair
              :desc "Insert Numbers" :nvg "n" #'mc/insert-numbers
              :desc "Sort Regions" :nvg "s" #'mc/sort-regions
              :desc "Reverse Regions" :nvg "t" #'mc/reverse-regions)))))

(map! (:leader (:prefix "x"
                 :desc "Zoom Text" :nvg "z" #'+hydra/text-zoom/body)))

(map! (:leader (:prefix "x"
                 (:prefix ("f" . "folding")
                   :when (featurep! :editor fold)
                   :desc "Close All" :nvg "C" #'+fold/close-all
                   :desc "Open All" :nvg "O" #'+fold/open-all
                   :desc "Close" :nvg "c" #'+fold/close
                   :desc "Open" :nvg "o" #'+fold/open
                   :desc "Toggle" :nvg "t" #'+fold/toggle
                   :desc "Next" :nvg "n" #'+fold/next
                   :desc "Previous" :nvg "p" #'+fold/previous))))

(map! (:leader (:prefix "n"
                 :desc "Narrow To Region" :nvg "r" #'narrow-to-region
                 :desc "Narrow To Defun" :nvg "d" #'narrow-to-defun
                 :desc "Narrow To Page" :nvg "p" #'narrow-to-page
                 :desc "Widen" :nvg "w" #'widen)))

(map! (:leader (:prefix "k"
        :desc "Barf Forward" :nvg "b" #'sp-forward-barf-sexp
        :desc "Barf Backward" :nvg "B" #'sp-backward-barf-sexp
        :desc "Slurp Forward" :nvg "s" #'sp-forward-slurp-sexp
        :desc "Slurp Backward" :nvg "S" #'sp-backward-slurp-sexp
        :desc "Absorb Sexp" :nvg "a" #'sp-absorb-sexp
        :desc "Convolute Sexp" :nvg "c" #'sp-convolute-sexp
        :desc "Previous Sexp" :nvg "p" #'sp-previous-sexp
        :desc "Next Sexp" :nvg "n" #'sp-next-sexp
        :desc "Transpose Sexp" :nvg "t" #'sp-transpose-sexp
        :desc "Split Sexp" :nvg "i" #'sp-split-sexp
        :desc "Splice Sexp" :nvg "I" #'sp-splice-sexp)))

(map! (:leader (:prefix "f"
                :desc "Find User Init File" :nvg "i" #'doom/goto-private-init-file)))

(map! (:leader
        :desc "Universal Argument" :nvg "u" #'universal-argument
        :desc "Undo" :nvg "." #'undo-fu-only-undo
        (:prefix "a"
         :desc "Dired" :nvg "d" #'dired
         (:when (featurep! :email mu4e)
          (:prefix ("m" . "mail")
           :desc "Compose" :nvg "c" #'+mu4e/compose
           :desc "Mail" :nvg "m" #'mu4e))
          (:prefix ("s" . "shell")
            (:when (featurep! :term vterm)
              :desc "Toggle vterm popup"    "v" #'+vterm/toggle
              :desc "Open vterm here"       "V" #'+vterm/here)
            (:when (featurep! :term eshell)
              :desc "Toggle eshell popup"   "e" #'+eshell/toggle
              :desc "Open eshell here"      "E" #'+eshell/here)))
        (:prefix "x"
          :desc "Count Region" :nvg "c" #'count-words-region
          :desc "Indent Rigidly" :nvg "TAB" #'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :nvg "c" #'transpose-chars
            :desc "Lines" :nvg "l" #'transpose-lines
            :desc "Words" :nvg "w" #'transpose-words))
        (:prefix "s"
          :desc "List Links" :nvg "L" #'ffap-menu)
        (:prefix "l"
         :desc "Uniquify Lines" :nvg "u" #'+bl/uniquify-lines-dwim
         :desc "Center Line" :nvg "c" #'recenter-top-bottom)
        (:prefix "b"
          :desc "Save" :nvg "s" #'save-buffer
          :desc "Scratch Buffer" :nvg "S" #'+bl/switch-to-scratch-buffer
          :desc "Messages Buffer" :nvg "M" #'+bl/switch-to-message-buffer
          :desc "New Buffer" :nvg "N" #'+bl/new-empty-buffer
          :desc "Buffer to Clipboard" :nvg "P" #'+bl/copy-whole-buffer-to-clipboard
          :desc "Clipboard to Buffer" :nvg "Y" #'+bl/copy-clipboard-to-whole-buffer
          :desc "Workspace Buffer" :nvg "w" #'+ivy/switch-workspace-buffer
          :desc "Read Only" :nvg "W" #'read-only-mode
          :desc "Next Buffer" :nvg "n" #'next-buffer
          :desc "Previous Buffer" :nvg "p" #'previous-buffer
          :desc "Doom Dashboard" :nvg "d" #'+doom-dashboard/open
          :desc "Doom Sandbox" :nvg "D" #'doom/sandbox
          :desc "Ibuffer" :nvg "I" #'ibuffer
          :desc "Kill Buffer and Window" :nvg "x" #'kill-buffer-and-window
          :desc "Kill Matching Buffers" :nvg "C-d" #'kill-matching-buffers
          :desc "Show and Copy Buffer Filename" :nvg "C" #'+bl/show-and-copy-buffer-filename)
        (:prefix "i"
          :desc "Insert Snippet" :nvg "s" #'yas-insert-snippet
          :desc "Insert Buffer" :nvg "b" #'insert-buffer)
        (:prefix "j"
          :desc "Open Line" :nvg "o" #'open-line
          :desc "New Line" :nvg "n" #'sp-newline
          :desc "Deer" :nvg "d" #'deer)
        (:prefix "f"
          (:prefix ("D" . "doom")
            :desc "Packages File" :nvg "p" #'doom/goto-packages-file
            :desc "Config File" :nvg "C" #'doom/goto-config-file
            :desc "Init file" :nvg "i" #'doom/goto-doomblock
            :desc "User Config" :nvg "c" #'doom/open-private-config))
        (:prefix "h"
          :desc "Version" :nvg "V" #'doom/version
          (:prefix ("t" . "tutorials")
            :desc "Emacs Tutorial" :nvg "e" #'help-with-tutorial)
          (:prefix "d"
           :desc "DOOM Autodefs" :nvg "A" #'doom/describe-autodefs
           :desc "Language Environment" :nvg "L" #'describe-language-environment
           :desc "Minor Mode(s)" :nvg "M" #'doom/describe-active-minor-mode
           :desc "DOOM Packages" :nvg "P" #'doom/help-packages
           :desc "DOOM Modules" :nvg "D" #'doom/help-modules))
        (:prefix "j"
          (:when (featurep! :ui window-select)
            :desc "Jump to Window" :nvg "w" #'ace-window))
        (:prefix "w"
          :desc "Save Session" :nvg "q" #'doom/quicksave-session
          :desc "Load Session" :nvg "Q" #'doom/quickload-session
          :desc "Make Frame" :nvg "F" #'make-frame
          :desc "Other Frame" :nvg "o" #'other-frame
          :desc "Winner Redo" :nvg "U" #'winner-redo
          :desc "Winner Undo" :nvg "u" #'winner-undo
          :desc "Split Window Right" :nvg "v" #'split-window-right
          :desc "Split Window Right & Focus" :nvg "V" #'split-window-right-and-focus
          :desc "Split Window Below" :nvg "s" #'split-window-below
          :desc "Split Window Below & Focus" :nvg "S" #'split-window-below-and-focus
          :desc "Balance Windows" :nvg "=" #'balance-windows
          :desc "Close Window" :nvg "k" #'+workspace/close-window-or-workspace
          :desc "Delete Other Windows" :nvg "K" #'delete-other-windows
          (:when (featurep! :ui window-select)
            :desc "Ace Window" :nvg "w" #'ace-window))
        (:prefix "K"
          :desc "Start or Insert Counter" :nvg "r" #'kmacro-start-macro-or-insert-counter
          :desc "End or Call Macro" :nvg "f" #'kmacro-end-or-call-macro-repeat
          :desc "Add Counter" :nvg "a" #'kmacro-add-counter
          :desc "Bind to Key" :nvg "b" #'kmacro-bind-to-key
          :desc "Call Macro" :nvg "c" #'kmacro-call-macro
          :desc "Delete Ring Head" :nvg "d" #'kmacro-delete-ring-head
          :desc "Insert Counter" :nvg "i" #'kmacro-insert-counter
          :desc "End Call Mouse" :nvg "m" #'kmacro-end-call-mouse
          :desc "Cycle Ring Next" :nvg "n" #'kmacro-cycle-ring-next
          :desc "Name Last Macro" :nvg "N" #'kmacro-name-last-macro
          :desc "Cycle Ring Previous" :nvg "p" #'kmacro-cycle-ring-previous
          :desc "View Macro" :nvg "v" #'kmacro-view-macro
          :desc "View Macro Repeat" :nvg "V" #'kmacro-view-macro-repeat
          :desc "Macro to Register" :nvg "w" #'kmacro-to-register
          :desc "Jump to Register" :nvg "y" #'jump-to-register
          (:prefix ("2" . "2nd")
            :desc "Call Ring 2nd" :nvg "c" #'kmacro-call-ring-2nd
            :desc "Call Ring 2nd Repeat" :nvg "C" #'kmacro-call-ring-2nd-repeat
            :desc "View Ring 2nd" :nvg "v" #'kmacro-view-ring-2nd)
          (:prefix ("e" . "edit")
            :desc "Edit Lossage" :nvg "l" #'kmacro-edit-lossage
            :desc "Edit Macro" :nvg "m" #'kmacro-edit-macro
            :desc "Edit Macro Repeat" :nvg "r" #'kmacro-edit-macro-repeat
            :desc "Step Edit Macro" :nvg "t" #'kmacro-step-edit-macro)
          (:prefix ("s" . "set")
            :desc "Set Counter" :nvg "c" #'kmacro-set-counter
            :desc "Set Format" :nvg "f" #'kmacro-set-format
            :desc "Swap Ring" :nvg "r" #'kmacro-swap-ring))
        (:prefix "R"
          :desc "Clear Rectangle" :nvg "!" #'clear-rectangle
          :desc "Close Rectangle" :nvg "c" #'close-rectangle
          :desc "Delete Rectangle" :nvg "d" #'delete-rectangle
          :desc "Exchange Point & Mark" :nvg "e" #'rectangle-exchange-point-and-mark
          :desc "Copy To Register" :nvg "i" #'copy-rectangle-to-register
          :desc "Kill Rectangle" :nvg "k" #'kill-rectangle
          :desc "Left Char" :nvg "l" #'rectangle-left-char
          :desc "Mark Mode" :nvg "m" #'rectangle-mark-mode
          :desc "Next Line" :nvg "n" #'rectangle-next-line
          :desc "Number Lines" :nvg "N" #'rectangle-number-lines
          :desc "Open Rectangle" :nvg "o" #'open-rectangle
          :desc "Previous Line" :nvg "p" #'rectangle-previous-line
          :desc "Right Char" :nvg "r" #'rectangle-right-char
          :desc "String Rectangle" :nvg "s" #'string-rectangle
          :desc "Transpose Regions" :nvg "t" #'transpose-regions
          :desc "Yank Rectangle" :nvg "y" #'yank-rectangle)
        (:prefix "E"
          (:prefix ("b" . "buffers")
            :desc "Buffers 3 Way" :nvg "3" #'ediff-buffers3
            :desc "Buffers" :nvg "b" #'ediff-buffers
            :desc "Patch Buffer" :nvg "p" #'ediff-patch-buffer)
          (:prefix ("d" . "directories")
            :desc "Directories 3 Way" :nvg "3" #'ediff-directories3
            :desc "Directories" :nvg "d" #'ediff-directories
            :desc "Directory Revisions" :nvg "r" #'ediff-directory-revisions)
          (:prefix ("f" . "files")
            :desc "File 3 Way" :nvg "3" #'ediff-files3
            :desc "Files" :nvg "f" #'ediff-files
            :desc "Patch File" :nvg "p" #'ediff-patch-file)
          (:prefix ("m" . "merge")
            (:prefix ("b" . "buffers")
              :desc "Merge Buffers with Ancestor" :nvg "3" #'ediff-merge-buffers-with-ancestor
              :desc "Merge Buffers" :nvg "b" #'ediff-merge-buffers)
            (:prefix ("d" . "directories")
              :desc "Merge Directories With Ancestor" :nvg "3" #'ediff-merge-directories-with-ancestor
              :desc "Merge Directories" :nvg "d" #'ediff-merge-directories)
            (:prefix ("f" . "files")
              :desc "Merge Files With Ancestor" :nvg "3" #'ediff-merge-files-with-ancestor
              :desc "Merge Files" :nvg "m" #'ediff-merge-files)
            (:prefix ("r" . "revisions")
              :desc "Merge Revisions With Ancestor" :nvg "3" #'ediff-merge-revisions-with-ancestor
              :desc "Merge Revisions" :nvg "r" #'ediff-merge-revisions))
          (:prefix ("r" . "regions")
            :desc "Diff Regions Linewise" :nvg "l" #'ediff-regions-linewise
            :desc "Diff Regions Wordwise" :nvg "w" #'ediff-regions-wordwise)
          (:prefix ("w" . "windows")
            :desc "Diff Windows Linewise" :nvg "l" #'ediff-windows-linewise
            :desc "Diff Windows Wordwise" :nvg "w" #'ediff-windows-wordwise)
          :desc "Backup" :nvg "B" #'ediff-backup
          :desc "Documentation" :nvg "h" #'ediff-documentation
          :desc "Show Registry" :nvg "s" #'ediff-show-registry
          :desc "Revisions" :nvg "r" #'ediff-revision)))

(map! (:leader
        :desc "Shell Command" :nvg "!" 'shell-command
        :desc "Eval" :nvg ":" 'eval-expression
        (:when (featurep! :tools ein)
          (:prefix "a"
            (:prefix ("y" . "jupyter notebooks")
              :desc "Login" :nvg "l" #'ein:notebooklist-login
              :desc "Open" :nvg "o" #'ein:notebooklist-open
              :desc "Run" :nvg "r" #'ein:run
              :desc "Stop" :nvg "s" #'ein:stop)))
        (:prefix "e"
          (:when (featurep! :checkers syntax)
            :desc "Next Error" :nvg "n" #'flycheck-next-error
            :desc "Previous Error" :nvg "p" #'flycheck-previous-error
            :desc "List Errors" :nvg "l" #'flycheck-list-errors
            :desc "Verify Setup" :nvg "v" #'flycheck-verify-setup))
        (:prefix "x"
          :desc "Downcase Region" :nvg "d" 'downcase-region
          :desc "Upcase Region" :nvg "u" 'upcase-region
          (:prefix ("i" . "indent")
            :desc "Indent Buffer" :nvg "b" #'+text/indent-buffer
            :desc "Indent Region" :nvg "r" #'indent-region
            :desc "Indent Region Or Buffer" :nvg "i" #'+text/indent-region-or-buffer))
        (:prefix "h"
          :desc "Emacs News" :nvg "n" #'view-emacs-news
          :desc "Doom Manual" :nvg "D" #'doom/help)
        (:prefix "q"
          :desc "Quit Emacs"                  :nvg "q" #'kill-emacs
          :desc "Save and quit Emacs"         :nvg "Q" #'save-buffers-kill-terminal
          :desc "Quick save current session"  :nvg "s" #'doom/quicksave-session
          :desc "Restore last session"        :nvg "l" #'doom/quickload-session
          :desc "Restart emacs server"        :nvg "d" #'+default/restart-server
          :desc "Delete frame"                :nvg "f" #'delete-frame
          :desc "Clear current frame"         :nvg "F" #'doom/kill-all-buffers
          :desc "Kill Emacs (and daemon)"     :nvg "K" #'save-buffers-kill-emacs
          (:when (featurep! :ui workspaces)
            :desc "Quit Emacs & forget session" :nvg "X" #'+workspace/kill-session-and-quit)
          :desc "Restart & restore Emacs"      :nvg "r" #'doom/restart-and-restore
          :desc "Restart Emacs"                :nvg "R" #'doom/restart)))


(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'recenter)

;; Remove binding, I did not need it and it was colliding with org mode keys
(after! pyenv-mode
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

;;; Make q close the window, not just the buffer
(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook (lambda () (define-key xwidget-webkit-mode-map  "q" #'+workspace/close-window-or-workspace))))
