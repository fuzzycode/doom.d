;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;
;;; <leader>

(map! :leader
      ";" nil ;; Save for later
      "x" nil ;; No need for scratch buffer, use for text instead
      "w" nil ;; I don't use window commands, use this for my needs
      "h" nil ;; I am used to my setup of help so I will use that

      ;; Remove deft keybinding if not using
      (:unless (featurep! :ui deft)
       (:prefix "n"
        "d" nil))

      ;; I do not use org clock feature so these can be removed
      (:prefix "n"
       "o" nil)

      (:when (featurep! :lang org)
       (:prefix "n"
        :desc "Org Roam Capture Today" "c" #'org-roam-dailies-capture-today
        :desc "Org Roam Capture" "C" #'org-roam-capture)) ;; Override Doom binding

      (:prefix ("j" . "jump")) ;; Claim the j prefix for me
      (:prefix ("x" . "text"))

      :desc "M-x" "<SPC>" #'execute-extended-command
      :desc "Eval Expression" ":" #'eval-expression
      :desc "Popup Scratch Buffer" "%" #'doom/open-scratch-buffer
      :desc "Shell Command" "!" #'shell-command

      ;; Insert
      (:prefix "i"
       (:when (featurep! :editor evil)
        :desc "New Line Above" "k" #'+evil/insert-newline-above
        :desc "New Line Below" "j" #'+evil/insert-newline-below))

      ;; Git
      (:prefix "g"
       (:when (featurep! :ui hydra)
        :desc "Blame" "B" #'+magit/blame-hydra/body
        :desc "Git Time Machine" "t" #'+magit/timemachine-hydra/body))

      ;; Toggle
      (:prefix "t"
       :desc "Trailing Whitespace" :ng "w" (cmd! (setq show-trailing-whitespace (not show-trailing-whitespace)))
       (:after lsp-mode
        :desc "Breadcrumb Mode" :ng "h" #'lsp-headerline-breadcrumb-mode))

      ;; Notes
      (:prefix "n"
       :desc "Open Project Todo File" "p" (cmd! (find-file (+org-capture-project-todo-file)))
       :desc "Open Project Notes File" "P" (cmd! (find-file (+org-capture-project-notes-file)))
       :desc "Open Global Todo File" "x" (cmd! (find-file (+org-capture-todo-file)))
       :desc "Open Global Notes File" "X" (cmd! (find-file (+org-capture-notes-file)))
       :desc "Global Project Todo File" "g" (cmd! (find-file (+org-capture-central-project-todo-file)))
       :desc "Global Project Notes File" "G" (cmd! (find-file (+org-capture-central-project-notes-file))))

      ;; Project
      (:prefix "p"
       "x" nil
       :desc "Scratch Buffer" "%" #'doom/open-project-scratch-buffer)

      ;; Help
      (:prefix "h"
       :desc "Info" "i" #'info
       :desc "Emacs News" "n" #'view-emacs-news
       :desc "Emacs Tutorial" "t" #'help-with-tutorial

       (:prefix ("a" . "apropos")
        :desc "Apropos" "a" #'apropos
        :desc "Command" "c" #'apropos-command
        :desc "Documentation" "d" #'apropos-documentation
        :desc "Documentation Property" "D" #'apropos-documentation-property
        :desc "Internal" "i" #'apropos-internal
        :desc "Library" "l" #'apropos-library
        :desc "Local Value" "V" #'apropos-local-value
        :desc "Local Variable" "E" #'apropos-local-variable
        :desc "Read Pattern" "r" #'apropos-read-pattern
        :desc "User Option" "o" #'apropos-user-option
        :desc "Value" "v" #'apropos-value
        :desc "Variable" "e" #'apropos-variable)
       (:prefix ("d" . "describe")
        :desc "Autodefs" "a" #'doom/describe-autodefs
        :desc "Char" "c" #'describe-char
        :desc "Bindings" "b" #'describe-bindings
        :desc "Doom Module" "d" #'doom/describe-module
        :desc "Function" "f" #'describe-function
        :desc "Face" "F" #'describe-face
        :desc "Key" "k" #'describe-key
        :desc "Language Environment" "L" #'describe-language-environment
        :desc "Mode" "m" #'describe-mode
        :desc "Active Mode" "M" #'doom/describe-active-minor-mode
        :desc "Symbol" "s" #'describe-symbol
        :desc "Package" "p" #'describe-package
        :desc "Doom Package" "P" #'doom/describe-package
        :desc "Theme" "t" #'describe-theme
        :desc "Variable" "v" #'describe-variable
        :desc "Text Properties" "T" #'describe-text-properties)
       (:prefix ("D" . "Doom")
        (:prefix ("b" . "Bump")
         :desc "Packages In Buffer" "b" #'doom/bump-packages-in-buffer
         :desc "Commit Bumps" "c" #'doom/commit-bumps
         :desc "Module" "m" #'doom/bump-module
         :desc "Package" "p" #'doom/bump-package
         :desc "Package At Point" "P" #'doom/bump-package-at-point)
        :desc "Info" "i" #'doom/info
        :desc "Issue Tracker" "I" #'doom/issue-tracker
        :desc "Homepage" "h" #'doom/homepage
        :desc "News" "n" #'doom/help-news
        :desc "Discourse" "d" #'doom/discourse
        :desc "Doom Manual" "D" #'doom/help
        :desc "Report Bug" "r" #'doom/report-bug
        :desc "Doom Reload" "R" #'doom/reload
        :desc "Version" "V" #'doom/version)))

;; Bindings with no leader key
(map!
 "<A-up>" #'join-line
 "<A-down>" (cmd! (delete-indentation 1)) ;; Top join line

 "C-x C-b" #'ibuffer
 "C-c l" #'recenter
 "C-c u" #'undo-fu-only-undo
 "C-j" #'evil-scroll-down
 "C-k" #'evil-scroll-up

 :ng "M-." #'+lookup/definition
 :n "q" nil

 (:after flyspell
  (:map flyspell-mode-map
   :ng "M-i" #'flyspell-correct-wrapper))
 (:after (projectile cc-mode)
  (:map c++-mode-map
   :n "go" #'projectile-find-other-file
   :ng "<A-tab>" #'projectile-find-other-file))
 (:after projectile
  :ng "M-o" #'projectile-find-file-dwim)
 (:after smartparens
  (:map smartparens-mode-map
   :ngi "C-<right>" #'sp-forward-slurp-sexp
   :ngi "C-<left>" #'sp-forward-barf-sexp
   :ngi "C-M-<right>" #'sp-backward-slurp-sexp
   :ngi "C-M-<left>" #'sp-backward-barf-sexp))
 (:after lsp-mode
  (:map lsp-mode-map
   :ngi "<A-return>" #'lsp-execute-code-action))
 (:after tabulated-list
  (:map tabulated-list-mode-map
   :ng "q" #'quit-window))
 (:after org-agenda
  (:map org-agenda-mode-map
   :ng "q" #'quit-window)))

(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook (lambda ()
                                        (define-key xwidget-webkit-mode-map (kbd "<up>") #'xwidget-webkit-scroll-up-line)
                                        (define-key xwidget-webkit-mode-map "<down>" #'xwidget-webkit-scroll-down-line)
                                        (define-key xwidget-webkit-mode-map  "q" #'+workspace/close-window-or-workspace))))
