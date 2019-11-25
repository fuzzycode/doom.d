
(map! (:leader
        (:prefix ("a" . "applications")
          :desc "Undo Tree" :g "u" #'undo-tree-visualize
          (:when (featurep! :email mu4e)
            :desc "Mail" :g "m" #'mu4e)
          (:prefix ("s" . "shell")
            (:when (featurep! :term vterm)
              :desc "Toggle vterm popup"    "v" #'+vterm/toggle
              :desc "Open vterm here"       "V" #'+vterm/here)
            (:when (featurep! :term eshell)
              :desc "Toggle eshell popup"   "e" #'+eshell/toggle
              :desc "Open eshell here"      "E" #'+eshell/here)))
        (:prefix ("x" . "text")
          :desc "Count Region" :g "c" 'count-words-region
          :desc "Indent Rigidly" :g "TAB" 'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :g "c" 'transpose-chars
            :desc "Lines" :g "l" 'transpose-lines
            :desc "Words" :g "w" 'transpose-words))
        (:prefix ("l" . "lines")
          :desc "Uniquify Lines" :g "u" #'+core/uniquify-lines-dwim)
        (:prefix ("b" . "buffer")
          :desc "Scratch Buffer" :g "S" '+core/switch-to-scratch-buffer
          :desc "Messages Buffer" :g "M" '+core/switch-to-message-buffer
          :desc "New Buffer" :g "N" '+core/new-empty-buffer
          :desc "Buffer to Clipboard" :g "P" '+core/copy-whole-buffer-to-clipboard
          :desc "Clipboard to Buffer" :g "Y" '+core/copy-clipboard-to-whole-buffer
          :desc "Read Only" :g "w" 'read-only-mode
          :desc "Next Buffer" :g "n" 'next-buffer
          :desc "Previous Buffer" :g "p" 'previous-buffer
          :desc "Doom Dashboard" :g "d" '+doom-dashboard/open
          :desc "Ibuffer" :g "I" 'ibuffer)
        (:prefix ("i" . "insert")
          :desc "Snippet" :g "s" #'yas-insert-snippet)
        (:prefix ("j" . "jump")
          :desc "Imenu" :g "i" #'imenu)
        (:prefix ("f" . "file")
          (:prefix ("D" . "doom")
            :desc "Packages File" :g "p" #'doom/goto-packages-file
            :desc "Config File" :g "C" #'doom/goto-config-file
            :desc "Init file" :g "i" #'doom/goto-doomblock
            :desc "User Config" :g "c" #'doom/open-private-config))
        (:prefix "h"
          :desc "Version" :g "V" #'doom/version
          (:prefix "d"
            :desc "Language Environment" :g "L" #'describe-language-environment
            :desc "Minor Mode(s)" :g "M" #'doom/describe-active-minor-mode
            :desc "DOOM Packages" :g "P" #'doom/help-packages
            :desc "DOOM Modules" :g "D" #'doom/help-modules))
        (:prefix ("w" . "windows")
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
          :desc "Balance Windows" :g "=" 'balance-windows)))
