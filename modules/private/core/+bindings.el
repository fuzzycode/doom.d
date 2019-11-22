
(map! (:leader
        (:prefix ("x" . "text")
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
