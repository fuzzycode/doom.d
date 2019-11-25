
;;;###autoload
(add-hook 'before-save-hook #'copyright-update)

;;;###autoload
(add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p)

;;;###autoload
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;;###autoload
(add-hook 'text-mode-hook #'flyspell-mode)
