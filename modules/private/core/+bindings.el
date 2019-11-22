
(map! (:leader
        (:prefix ("x" . "text")
          :desc "Count Region" :g "c" 'count-words-region
          :desc "Indent Rigidly" :g "TAB" 'indent-rigidly
          (:prefix ("t" . "transpose")
            :desc "Chars" :g "c" 'transpose-chars
            :desc "Lines" :g "l" 'transpose-lines
            :desc "Words" :g "w" 'transpose-words))
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
        (:prefix ("j" . "jump")
          :desc "Find Function" :g "f" 'find-function
          :desc "Find Variable" :g "v" 'find-variable)
        (:prefix ("w" . "windows")
          :desc "Make Frame" :g "F" 'make-frame
          :desc "Other Frame" :g "o" 'other-frame
          :desc "Winner Redo" :g "U" 'winner-redo
          :desc "Winner Undo" :g "u" 'winner-undo
          :desc "Split Window Right" :g "v" 'split-window-right
          :desc "Split Window Right & Focus" :g "V" 'split-window-right-and-focus
          :desc "Split WIndow Below" :g "s" 'split-window-below
          :desc "Split WIndow Below & Focus" :g "S" 'split-window-below-and-focus
          :desc "Balance Windows" :g "=" 'balance-windows)))
