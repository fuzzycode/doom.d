;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;
;;; <leader>

(map! :leader
      ";" nil ;; Save for later
      "x" nil ;; No need for scratch buffer, use for text instead
      "w" nil ;; I don't use window commands, use this for my needs
      "h" nil ;; I am used to my setup of help so I will use that

      :desc "M-x" "<SPC>" #'execute-extended-command
      :desc "Eval Expression" ":" #'eval-expression
      :desc "Popup Scratch Buffer" "%" #'doom/open-scratch-buffer
      :desc "Shell Command" "!" #'shell-command

      ;; Toggle
      (:prefix "t"
       :desc "Trailing Whitespace" :ng "w" (cmd! (setq show-trailing-whitespace (not show-trailing-whitespace)))
       (:after lsp-mode
         :desc "Breadcrumb Mode" :ng "h" #'lsp-headerline-breadcrumb-mode)))

;; Bindings with no leader key
(map!
 "<A-up>" #'join-line
 "<A-down>" (cmd! (delete-indentation 1)) ;; Top join line

 "C-x C-b" #'ibuffer
 "C-c l" #'recenter
 "C-c u" #'undo-fu-only-undo
 "C-j" #'evil-scroll-down
 "C-k" #'evil-scroll-up

 :g "M-." #'+lookup/definition
 :n "q" nil

 (:after flyspell
  (:map flyspell-mode-map
   :ng "M-i" #'flyspell-correct-wrapper))
 (:after (projectile cc-mode)
  (:map c++-mode-map
   :ng "<A-tab>" #'projectile-find-other-file))
 (:after projectile
  :ng "M-o" #'projectile-find-file-dwim)
 (:after smartparens
  (:map smartparens-mode-map
   :ngi "C-<right>" #'sp-forward-slurp-sexp
   :ngi "C-<left>" #'sp-forward-barf-sexp
   :ngi "C-M-<right>" #'sp-backward-slurp-sexp
   :ngi "C-M-<left>" #'sp-backward-barf-sexp
   :ngi "C-M-s" #'smartparens-hydra/body))
 (:after lsp-mode
  (:map lsp-mode-map
   :ngi "<A-return>" #'lsp-execute-code-action))

 (:after projectile
  "M-o" #'projectile-find-file-dwim
  :n "go" #'projectile-find-other-file))

(when (featurep 'xwidget-internal)
  (add-hook 'xwidget-webkit-mode-hook (lambda ()
                                        (define-key xwidget-webkit-mode-map (kbd "<up>") #'xwidget-webkit-scroll-up-line)
                                        (define-key xwidget-webkit-mode-map "<down>" #'xwidget-webkit-scroll-down-line)
                                        (define-key xwidget-webkit-mode-map  "q" #'+workspace/close-window-or-workspace))))
