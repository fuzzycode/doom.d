;; -*- lexical-binding: t; -*-
;;;###if (modulep! :tools lsp)

;;;###autoload
(defun +bl/dim-lsp-sideline (&rest args)
  (mapcar (lambda (face)
           (when (facep face)
             (set-face-foreground face (face-attribute 'font-lock-comment-face :foreground))))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info)))

;;;###autoload
(defun +bl/toggle-global-bredcrumb-mode-on ()
  "Turn on global lsp breadcrumb mode"
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  (lsp-headerline-breadcrumb-mode t))

;;;###autoload
(defun +bl/toggle-global-bredcrumb-mode-off ()
  "Turn off global lsp breadcrumb mode"
  (remove-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  (lsp-headerline-breadcrumb-mode -1))

;;;###autoload
(defun +bl/toggle-header-source ()
  "Toggle between header and source file using LSP if supported."
  (interactive)
  (if (and (derived-mode-p 'c++-mode 'c-mode) (bound-and-true-p lsp-mode)) ;; This assumes I always use clangd with c++ code
      (lsp-clangd-find-other-file)
    (projectile-find-other-file)))
