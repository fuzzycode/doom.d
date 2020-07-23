
;;;###autoload
(defun +lsp/dim-lsp-sideline (&rest args)
  (mapcar (lambda (face)
           (when (facep face)
             (set-face-foreground face (face-attribute 'font-lock-comment-face :foreground))))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info)))

;;;###autoload
(defun +lsp/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;;###autoload

;;;###autoload
