
(map! (:leader
        :desc "Shell Command" :g "!" 'shell-command
        :desc "Eval" :g ":" 'eval-expression
        (:prefix ("x" . "text")
          :desc "Downcase Region" :g "d" 'downcase-region
          :desc "Upcase Region" :g "u" 'upcase-region)
        (:prefix ("h" . "help")
          :desc "Emacs News" :g "n" 'emacs-news
          :desc "Doom Manual" :g "D" 'doom/help)
        (:prefix ("q" . "quit/reload")
          :desc "Quit Emacs"                   "q" #'kill-emacs
          :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
          (:when (featurep! :ui workspaces)
            :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit)
          :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
          :desc "Restart Emacs"                "R" #'doom/restart)))
