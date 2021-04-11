
(map! (:localleader
        :mode (c++-mode python-mode)
        (:prefix ("=" . "format")
          :desc "Format Dwim" :ng "=" #'+lsp/lsp-format-region-or-buffer
          :desc "Format Buffer" :ng "b" #'lsp-format-buffer
          :desc "Format Region" :ng "r" #'lsp-format-region)
        (:prefix ("r" . "refactor")
          :desc "Rename" :ng "r" #'lsp-rename)))

(after! lsp-ui
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline))

(setq lsp-enable-semantic-highlighting t) ; Enable semantic highlighting by default

(when (featurep! :completion ivy)
  (setq lsp-ivy-show-symbol-filename nil)) ; remove the file path from workspace symbols

;;;###package
(use-package! lsp-treemacs
  :after lsp-mode
  :init (map! (:leader (:prefix "e"
                        :desc "All Errors" :ng "a" #'lsp-treemacs-errors-list)))
  (set-popup-rule! "^\\*LSP Error List\\*$" :size 0.5 :side 'bottom :select t :ttl nil))
