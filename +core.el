
(map! (:leader
        :desc "Shell Command" :g "!" 'shell-command
        :desc "Eval" :g ":" 'eval-expression
        (:when (featurep! :tools ein)
          (:prefix ("a" . "applications")
            (:prefix ("y" . "jupyter notebooks")
              :desc "Login" :g "l" #'ein:notebooklist-login
              :desc "Open" :g "o" #'ein:notebooklist-open
              :desc "Run" :g "r" #'ein:run
              :desc "Stop" :g "s" #'ein:stop)))
        (:prefix ("S" . "spelling")
            (:when (featurep! :tools flyspell)
              :desc "Correct previous word" :g "c" #'flyspell-correct-previous-word-generic
              :desc "Check Buffer" :g "b" #'flyspell-buffer
              :desc "Change Dictionary" :g "C" #'ispell-change-dictionary))
        (:prefix ("e" . "errors")
          (:when (featurep! :tools flycheck)
            :desc "Next Error" :g "n" #'flycheck-next-error
            :desc "Previous Error" :g "p" #'flycheck-previous-error
            :desc "List Errors" :g "l" #'flycheck-list-errors))
        (:prefix ("x" . "text")
          :desc "Downcase Region" :g "d" 'downcase-region
          :desc "Upcase Region" :g "u" 'upcase-region
          (:prefix ("i" . "indent")
            :desc "Indent Buffer" :g "b" #'+text/indent-buffer
            :desc "Indent Region" :g "r" #'indent-region
            :desc "Indent Region Or Buffer" :g "i" #'+text/indent-region-or-buffer))
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

(setq which-key-sort-order 'which-key-key-order-alpha)

(global-set-key (kbd "C-c u") 'undo)
