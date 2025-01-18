;;; ../Workspace/dotfiles/doom.d/autoload/+capf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/capf-fallback ()
  (cape-capf-super
   #'cape-keyword
   #'cape-dabbrev))

;;;###autoload
(defun +bl/capf-setup:emacs-lisp ()
  (list #'cape-file
        (cape-capf-inside-comment #'cape-dict)
        (cape-capf-inside-string #'cape-dict)
        (cape-capf-super #'elisp-completion-at-point #'yasnippet-capf)
        (+bl/capf-fallback)))

;;;###autoload
(defun +bl/capf-setup:lsp ()
  (list #'cape-file
        (cape-capf-inside-comment #'cape-dict)
        (cape-capf-inside-string #'cape-dict)
        (cape-capf-super
         (if (modulep! :lsp +eglot) #'eglot-completion-at-point
           #'lsp-completion-at-point) #'yasnippet-capf)
        (+bl/capf-fallback)))

;;;###autoload
(defun +bl/capf-setup:org ()
  (list #'cape-file
        #'cape-elisp-block
        #'org-block-capf
        (cape-capf-super
         #'yasnippet-capf
         #'cape-dict)
        (+bl/capf-fallback)))

;;;###autoload
(defun +bl/capf-setup:comint ()
  (list #'cape-history
        #'cape-dict
        (+bl/capf-fallback)))


;;;###autoload
(add-hook 'lsp-managed-mode-hook (lambda () (setq-local completion-at-point-functions (+bl/capf-setup:lsp))))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local completion-at-point-functions (+bl/capf-setup:emacs-lisp))))

;;;###autoload
(add-hook 'org-mode-hook (lambda () (setq-local completion-at-point-functions (+bl/capf-setup:org))))

;;;###autoload
(add-hook 'comint-mode-hook (lambda () (setq-local completion-at-point-functions (+bl/capf-setup:comint))))
