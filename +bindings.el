;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
       (:prefix "h"
          :desc "Info" :g "i" #'info
          (:prefix ("d" . "describe")
            :desc "Char" :g "c" #'describe-char
            :desc "Bindings" :g "b" #'describe-bindings
            :desc "Function" :g "f" #'describe-function
            :desc "Face" :g "F" #'describe-face
            :desc "Key" :g "k" #'describe-key
            :desc "Mode" :g "m" #'describe-mode
            :desc "Symbol" :g "s" #'describe-symbol
            :desc "Package" :g "p" #'describe-package
            :desc "Theme" :g "t" #'describe-theme
            :desc "Variable" :g "v" #'describe-variable
            :desc "Text Properties" :g "T" #'describe-text-properties))))

(map! (:leader
        (:prefix "s"
          (:prefix ("m" . "multiple cursors")
            :desc "Mark All" :g "a" #'mc/mark-all-dwim
            :desc "Mark All Like This" :g "b" #'mc/mark-all-like-this
            :desc "Mark More Like This" :g "m" #'mc/mark-more-like-this-extended
            :desc "Edit Lines" :g "r" #'mc/edit-lines
            (:prefix ("s" . "insert/sort")
              :desc "Inert Letters" :g "l" #'mc/insert-letters
              :desc "Mark SGML pair" :g "m" #'mc/mark-sgml-tag-pair
              :desc "Insert Numbers" :g "n" #'mc/insert-numbers
              :desc "Sort Regions" :g "s" #'mc/sort-regions
              :desc "Reverse Regions" :g "t" #'mc/reverse-regions)))))

(map! (:leader (:prefix "x"
                 :desc "Zoom Text" :g "z" #'+hydra/text-zoom/body)))

(map! (:leader (:prefix "x"
                 (:prefix ("f" . "folding")
                   :when (featurep! :editor fold)
                   :desc "Close All" :g "C" #'+fold/close-all
                   :desc "Open All" :g "O" #'+fold/open-all
                   :desc "Close" :g "c" #'+fold/close
                   :desc "Open" :g "o" #'+fold/open
                   :desc "Toggle" :g "t" #'+fold/toggle
                   :desc "Next" :g "n" #'+fold/next
                   :desc "Previous" :g "p" #'+fold/previous))))

(map! (:leader (:prefix "n"
                 :desc "Narrow To Region" :g "r" #'narrow-to-region
                 :desc "Narrow To Defun" :g "d" #'narrow-to-defun
                 :desc "Narrow To Page" :g "p" #'narrow-to-page
                 :desc "Widen" :g "w" #'widen)))

(map! (:leader (:prefix "k"
        :desc "Barf Forward" :g "b" #'sp-forward-barf-sexp
        :desc "Barf Backward" :g "B" #'sp-backward-barf-sexp
        :desc "Slurp Forward" :g "s" #'sp-forward-slurp-sexp
        :desc "Slurp Backward" :g "S" #'sp-backward-slurp-sexp
        :desc "Absorb Sexp" :g "a" #'sp-absorb-sexp
        :desc "Convolute Sexp" :g "c" #'sp-convolute-sexp
        :desc "Previous Sexp" :g "p" #'sp-previous-sexp
        :desc "Next Sexp" :g "n" #'sp-next-sexp
        :desc "Transpose Sexp" :g "t" #'sp-transpose-sexp
        :desc "Split Sexp" :g "i" #'sp-split-sexp
        :desc "Splice Sexp" :g "I" #'sp-splice-sexp)))

(map! (:leader (:prefix "f"
                :desc "Find User Init File" :g "i" #'doom/goto-private-init-file)))

(map! (:leader
        :desc "Universal Argument" :g "u" #'universal-argument
        :desc "Undo" :g "." #'undo-fu-only-undo
        (:prefix "a"
         :desc "Dired" :g "d" #'dired
         (:when (featurep! :email mu4e)
          (:prefix ("m" . "mail")
           :desc "Compose" :g "c" #'+mu4e/compose
           :desc "Mail" :g "m" #'mu4e))
          (:prefix ("s" . "shell")
            (:when (featurep! :term vterm)
              :desc "Toggle vterm popup"    "v" #'+vterm/toggle
              :desc "Open vterm here"       "V" #'+vterm/here)
            (:when (featurep! :term eshell)
              :desc "Toggle eshell popup"   "e" #'+eshell/toggle
              :desc "Open eshell here"      "E" #'+eshell/here)))
        (:prefix "x"
          :desc "Count Region" :g "c" #'count-words-region
          :desc "Indent Rigidly" :g "TAB" #'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :g "c" #'transpose-chars
            :desc "Lines" :g "l" #'transpose-lines
            :desc "Words" :g "w" #'transpose-words))
        (:prefix "s"
          :desc "List Links" :g "L" #'ffap-menu)
        (:prefix "l"
         :desc "Uniquify Lines" :g "u" #'+bl/uniquify-lines-dwim
         :desc "Center Line" :g "c" #'recenter-top-bottom)
        (:prefix "b"
          :desc "Save" :g "s" #'save-buffer
          :desc "Scratch Buffer" :g "S" #'+bl/switch-to-scratch-buffer
          :desc "Messages Buffer" :g "M" #'+bl/switch-to-message-buffer
          :desc "New Buffer" :g "N" #'+bl/new-empty-buffer
          :desc "Buffer to Clipboard" :g "P" #'+bl/copy-whole-buffer-to-clipboard
          :desc "Clipboard to Buffer" :g "Y" #'+bl/copy-clipboard-to-whole-buffer
          :desc "Workspace Buffer" :g "w" #'+ivy/switch-workspace-buffer
          :desc "Read Only" :g "W" #'read-only-mode
          :desc "Next Buffer" :g "n" #'next-buffer
          :desc "Previous Buffer" :g "p" #'previous-buffer
          :desc "Doom Dashboard" :g "d" #'+doom-dashboard/open
          :desc "Doom Sandbox" :g "D" #'doom/sandbox
          :desc "Ibuffer" :g "I" #'ibuffer
          :desc "Kill Buffer and Window" :g "x" #'kill-buffer-and-window
          :desc "Kill Matching Buffers" :g "C-d" #'kill-matching-buffers
          :desc "Show and Copy Buffer Filename" :g "C" #'+bl/show-and-copy-buffer-filename)
        (:prefix "i"
          :desc "Insert Snippet" :g "s" #'yas-insert-snippet
          :desc "Insert Buffer" :g "b" #'insert-buffer)
        (:prefix "j"
          :desc "Open Line" :g "o" #'open-line
          :desc "New Line" :g "n" #'sp-newline
          :desc "Deer" :g "d" #'deer)
        (:prefix "f"
          (:prefix ("D" . "doom")
            :desc "Packages File" :g "p" #'doom/goto-packages-file
            :desc "Config File" :g "C" #'doom/goto-config-file
            :desc "Init file" :g "i" #'doom/goto-doomblock
            :desc "User Config" :g "c" #'doom/open-private-config))
        (:prefix "h"
          :desc "Version" :g "V" #'doom/version
          (:prefix ("t" . "tutorials")
            :desc "Emacs Tutorial" :g "e" #'help-with-tutorial)
          (:prefix "d"
           :desc "DOOM Autodefs" :g "A" #'doom/describe-autodefs
           :desc "Language Environment" :g "L" #'describe-language-environment
           :desc "Minor Mode(s)" :g "M" #'doom/describe-active-minor-mode
           :desc "DOOM Packages" :g "P" #'doom/help-packages
           :desc "DOOM Modules" :g "D" #'doom/help-modules))
        (:prefix "j"
          (:when (featurep! :ui window-select)
            :desc "Jump to Window" :g "w" #'ace-window))
        (:prefix "w"
          :desc "Save Session" :g "q" #'doom/quicksave-session
          :desc "Load Session" :g "Q" #'doom/quickload-session
          :desc "Make Frame" :g "F" #'make-frame
          :desc "Other Frame" :g "o" #'other-frame
          :desc "Winner Redo" :g "U" #'winner-redo
          :desc "Winner Undo" :g "u" #'winner-undo
          :desc "Split Window Right" :g "v" #'split-window-right
          :desc "Split Window Right & Focus" :g "V" #'split-window-right-and-focus
          :desc "Split Window Below" :g "s" #'split-window-below
          :desc "Split Window Below & Focus" :g "S" #'split-window-below-and-focus
          :desc "Balance Windows" :g "=" #'balance-windows
          :desc "Close Window" :g "k" #'+workspace/close-window-or-workspace
          :desc "Delete Other Windows" :g "K" #'delete-other-windows
          (:when (featurep! :ui window-select)
            :desc "Ace Window" :g "w" #'ace-window))
        (:prefix "K"
          :desc "Start or Insert Counter" :g "r" #'kmacro-start-macro-or-insert-counter
          :desc "End or Call Macro" :g "f" #'kmacro-end-or-call-macro-repeat
          :desc "Add Counter" :g "a" #'kmacro-add-counter
          :desc "Bind to Key" :g "b" #'kmacro-bind-to-key
          :desc "Call Macro" :g "c" #'kmacro-call-macro
          :desc "Delete Ring Head" :g "d" #'kmacro-delete-ring-head
          :desc "Insert Counter" :g "i" #'kmacro-insert-counter
          :desc "End Call Mouse" :g "m" #'kmacro-end-call-mouse
          :desc "Cycle Ring Next" :g "n" #'kmacro-cycle-ring-next
          :desc "Name Last Macro" :g "N" #'kmacro-name-last-macro
          :desc "Cycle Ring Previous" :g "p" #'kmacro-cycle-ring-previous
          :desc "View Macro" :g "v" #'kmacro-view-macro
          :desc "View Macro Repeat" :g "V" #'kmacro-view-macro-repeat
          :desc "Macro to Register" :g "w" #'kmacro-to-register
          :desc "Jump to Register" :g "y" #'jump-to-register
          (:prefix ("2" . "2nd")
            :desc "Call Ring 2nd" :g "c" #'kmacro-call-ring-2nd
            :desc "Call Ring 2nd Repeat" :g "C" #'kmacro-call-ring-2nd-repeat
            :desc "View Ring 2nd" :g "v" #'kmacro-view-ring-2nd)
          (:prefix ("e" . "edit")
            :desc "Edit Lossage" :g "l" #'kmacro-edit-lossage
            :desc "Edit Macro" :g "m" #'kmacro-edit-macro
            :desc "Edit Macro Repeat" :g "r" #'kmacro-edit-macro-repeat
            :desc "Step Edit Macro" :g "t" #'kmacro-step-edit-macro)
          (:prefix ("s" . "set")
            :desc "Set Counter" :g "c" #'kmacro-set-counter
            :desc "Set Format" :g "f" #'kmacro-set-format
            :desc "Swap Ring" :g "r" #'kmacro-swap-ring))
        (:prefix "R"
          :desc "Clear Rectangle" :g "!" #'clear-rectangle
          :desc "Close Rectangle" :g "c" #'close-rectangle
          :desc "Delete Rectangle" :g "d" #'delete-rectangle
          :desc "Exchange Point & Mark" :g "e" #'rectangle-exchange-point-and-mark
          :desc "Copy To Register" :g "i" #'copy-rectangle-to-register
          :desc "Kill Rectangle" :g "k" #'kill-rectangle
          :desc "Left Char" :g "l" #'rectangle-left-char
          :desc "Mark Mode" :g "m" #'rectangle-mark-mode
          :desc "Next Line" :g "n" #'rectangle-next-line
          :desc "Number Lines" :g "N" #'rectangle-number-lines
          :desc "Open Rectangle" :g "o" #'open-rectangle
          :desc "Previous Line" :g "p" #'rectangle-previous-line
          :desc "Right Char" :g "r" #'rectangle-right-char
          :desc "String Rectangle" :g "s" #'string-rectangle
          :desc "Transpose Regions" :g "t" #'transpose-regions
          :desc "Yank Rectangle" :g "y" #'yank-rectangle)
        (:prefix "E"
          (:prefix ("b" . "buffers")
            :desc "Buffers 3 Way" :g "3" #'ediff-buffers3
            :desc "Buffers" :g "b" #'ediff-buffers
            :desc "Patch Buffer" :g "p" #'ediff-patch-buffer)
          (:prefix ("d" . "directories")
            :desc "Directories 3 Way" :g "3" #'ediff-directories3
            :desc "Directories" :g "d" #'ediff-directories
            :desc "Directory Revisions" :g "r" #'ediff-directory-revisions)
          (:prefix ("f" . "files")
            :desc "File 3 Way" :g "3" #'ediff-files3
            :desc "Files" :g "f" #'ediff-files
            :desc "Patch File" :g "p" #'ediff-patch-file)
          (:prefix ("m" . "merge")
            (:prefix ("b" . "buffers")
              :desc "Merge Buffers with Ancestor" :g "3" #'ediff-merge-buffers-with-ancestor
              :desc "Merge Buffers" :g "b" #'ediff-merge-buffers)
            (:prefix ("d" . "directories")
              :desc "Merge Directories With Ancestor" :g "3" #'ediff-merge-directories-with-ancestor
              :desc "Merge Directories" :g "d" #'ediff-merge-directories)
            (:prefix ("f" . "files")
              :desc "Merge Files With Ancestor" :g "3" #'ediff-merge-files-with-ancestor
              :desc "Merge Files" :g "" #'ediff-merge-files)
            (:prefix ("r" . "revisions")
              :desc "Merge Revisions With Ancestor" :g "3" #'ediff-merge-revisions-with-ancestor
              :desc "Merge Revisions" :g "r" #'ediff-merge-revisions))
          (:prefix ("r" . "regions")
            :desc "Diff Regions Linewise" :g "l" #'ediff-regions-linewise
            :desc "Diff Regions Wordwise" :g "w" #'ediff-regions-wordwise)
          (:prefix ("w" . "windows")
            :desc "Diff Windows Linewise" :g "l" #'ediff-windows-linewise
            :desc "Diff Windows Wordwise" :g "w" #'ediff-windows-wordwise)
          :desc "Backup" :g "B" #'ediff-backup
          :desc "Documentation" :g "h" #'ediff-documentation
          :desc "Show Registry" :g "s" #'ediff-show-registry
          :desc "Revisions" :g "r" #'ediff-revision)))

(map! (:leader
        :desc "Shell Command" :g "!" 'shell-command
        :desc "Eval" :g ":" 'eval-expression
        (:when (featurep! :tools ein)
          (:prefix "a"
            (:prefix ("y" . "jupyter notebooks")
              :desc "Login" :g "l" #'ein:notebooklist-login
              :desc "Open" :g "o" #'ein:notebooklist-open
              :desc "Run" :g "r" #'ein:run
              :desc "Stop" :g "s" #'ein:stop)))
        (:prefix "e"
          (:when (featurep! :checkers syntax)
            :desc "Next Error" :g "n" #'flycheck-next-error
            :desc "Previous Error" :g "p" #'flycheck-previous-error
            :desc "List Errors" :g "l" #'flycheck-list-errors
            :desc "Verify Setup" :g "v" #'flycheck-verify-setup))
        (:prefix "x"
          :desc "Downcase Region" :g "d" 'downcase-region
          :desc "Upcase Region" :g "u" 'upcase-region
          (:prefix ("i" . "indent")
            :desc "Indent Buffer" :g "b" #'+text/indent-buffer
            :desc "Indent Region" :g "r" #'indent-region
            :desc "Indent Region Or Buffer" :g "i" #'+text/indent-region-or-buffer))
        (:prefix "h"
          :desc "Emacs News" :g "n" #'view-emacs-news
          :desc "Doom Manual" :g "D" #'doom/help)
        (:prefix "q"
          :desc "Quit Emacs"                   "q" #'kill-emacs
          :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
          :desc "Quick save current session"   "s" #'doom/quicksave-session
          :desc "Restore last session"         "l" #'doom/quickload-session
          :desc "Restart emacs server"         "d" #'+default/restart-server
          :desc "Delete frame"                 "f" #'delete-frame
          :desc "Clear current frame"          "F" #'doom/kill-all-buffers
          :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
          (:when (featurep! :ui workspaces)
            :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit)
          :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
          :desc "Restart Emacs"                "R" #'doom/restart)))


(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'recenter)

;; Remove binding, I did not need it and it was colliding with org mode keys
(after! pyenv-mode
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))

;;; Make q close the window, not just the buffer
(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook (lambda () (define-key xwidget-webkit-mode-map  "q" #'+workspace/close-window-or-workspace))))
