;;; -*- lexical-binding: t; -*-

;;;###autoload
(add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p)

;;;###autoload
(add-hook 'help-mode-hook #'rainbow-mode)

;;;###autoload
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type 'relative)))

