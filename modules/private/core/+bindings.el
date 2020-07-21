;; -*- lexical-binding: t; -*-

(map! (:leader
        :desc "Universal Argument" :g "u" #'universal-argument
        :desc "Undo" :g "." #'undo-fu-only-undo
        (:prefix "a"
         (:when (featurep! :email mu4e)
          (:prefix ("m" "+mail")
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
          :desc "Count Region" :g "c" 'count-words-region
          :desc "Indent Rigidly" :g "TAB" 'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :g "c" 'transpose-chars
            :desc "Lines" :g "l" 'transpose-lines
            :desc "Words" :g "w" 'transpose-words))
        (:prefix "s"
          :desc "List Links" :g "L" #'ffap-menu)
        (:prefix "l"
         :desc "Uniquify Lines" :g "u" #'+core/uniquify-lines-dwim
         :desc "Center Line" :g "c" #'recenter-top-bottom)
        (:prefix "b"
          :desc "Save" :g "s" #'save-buffer
          :desc "Scratch Buffer" :g "S" '+core/switch-to-scratch-buffer
          :desc "Messages Buffer" :g "M" '+core/switch-to-message-buffer
          :desc "New Buffer" :g "N" '+core/new-empty-buffer
          :desc "Buffer to Clipboard" :g "P" '+core/copy-whole-buffer-to-clipboard
          :desc "Clipboard to Buffer" :g "Y" '+core/copy-clipboard-to-whole-buffer
          :desc "Workspace Buffer" :g "w" '+ivy/switch-workspace-buffer
          :desc "Read Only" :g "W" 'read-only-mode
          :desc "Next Buffer" :g "n" 'next-buffer
          :desc "Previous Buffer" :g "p" 'previous-buffer
          :desc "Doom Dashboard" :g "d" '+doom-dashboard/open
          :desc "Ibuffer" :g "I" 'ibuffer
          :desc "Kill Buffer and Window" :g "x" #'kill-buffer-and-window
          :desc "Kill Matching Buffers" :g "C-d" #'kill-matching-buffers
          :desc "Show and Copy Buffer Filename" :g "C" #'+core/show-and-copy-buffer-filename)
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
          :desc "Make Frame" :g "F" 'make-frame
          :desc "Other Frame" :g "o" 'other-frame
          :desc "Winner Redo" :g "U" 'winner-redo
          :desc "Winner Undo" :g "u" 'winner-undo
          :desc "Split Window Right" :g "v" 'split-window-right
          :desc "Split Window Right & Focus" :g "V" 'split-window-right-and-focus
          :desc "Split Window Below" :g "s" 'split-window-below
          :desc "Split Window Below & Focus" :g "S" 'split-window-below-and-focus
          :desc "Balance Windows" :g "=" 'balance-windows
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
