
(map! (:localleader
        :mode (c++-mode python-mode)
        (:prefix ("=" . "format")
          :desc "Format Dwim" :ng "=" #'+lsp/lsp-format-region-or-buffer
          :desc "Format Buffer" :ng "b" #'lsp-format-buffer
          :desc "Format Region" :ng "r" #'lsp-format-region)
        (:prefix ("r" . "refactor")
          :desc "Rename" :ng "r" #'lsp-rename)))

(after! lsp-mode
  (define-key lsp-mode-map (kbd "<A-return>") #'lsp-execute-code-action)

  (+core/add-toggle breadcrum-headline
                    :status lsp-headerline-breadcrumb-mode
                    :on (+lsp/toggle-global-bredcrumb-mode-on)
                    :on-message "Global Breadcrumb enabled"
                    :off (+lsp/toggle-global-bredcrumb-mode-off)
                    :off-message "Global Breadcrumb disabled"
                    :bind '(:desc "Breadcrumb Mode" :key "h")))

(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline))

(setq dap-breakpoints-file (concat doom-local-dir "cache/dap-breakpoints"))

;;;###package
(use-package! lsp-treemacs
  :after lsp-mode
  :init (map! (:leader (:prefix "e"
                        :desc "All Errors" :ng "a" #'lsp-treemacs-errors-list)))
  (set-popup-rule! "^\\*LSP Error List\\*$" :size 0.5 :side 'bottom :select t :ttl nil))
