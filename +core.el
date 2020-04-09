;; -*- lexical-binding: t; -*-

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
        (:prefix "S"
            (:when (featurep! :tools flyspell)
              :desc "Check Buffer" :g "b" #'flyspell-buffer
              :desc "Change Dictionary" :g "C" #'ispell-change-dictionary))
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
          :desc "Emacs News" :g "n" 'emacs-news
          :desc "Doom Manual" :g "D" 'doom/help)
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

(setq which-key-sort-order 'which-key-key-order-alpha)

(global-set-key (kbd "C-c u") #'undo-fu-only-undo)

(add-hook 'help-mode-hook #'rainbow-mode)

(setq projectile-enable-caching nil)

(after! yasnippet
  (when (file-exists-p "~/.snippets")
    (add-to-list 'yas-snippet-dirs "~/.snippets")
    (yas-reload-all)))

(after! flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                     ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                     ("Col" 3 nil :right-align t)
                                     ("Level" 8 flycheck-error-list-entry-level-<)
                                     ("ID" 35 t)
                                     (#("Message (Checker)" 9 16
                                        (face flycheck-error-list-checker-name))
                                      0 t)]))

(setq ibuffer-formats '((mark modified read-only locked " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))
