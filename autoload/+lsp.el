
;;;###autoload
(defun +lsp/dim-lsp-sideline (&rest args)
  (mapcar (lambda (face)
           (when (facep face)
             (set-face-foreground face (face-attribute 'font-lock-comment-face :foreground))))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info)))

;;;###autoload
(defun +lsp/toggle-global-bredcrumb-mode-on ()
  "Turn on global lsp breadcrumb mode"
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  (lsp-headerline-breadcrumb-mode t))

;;;###autoload
(defun +lsp/toggle-global-bredcrumb-mode-off ()
  "Turn off global lsp breadcrumb mode"
  (remove-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  (lsp-headerline-breadcrumb-mode -1))
