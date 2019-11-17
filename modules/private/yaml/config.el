
(use-package! yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("\\.clang-format\\'" . yaml-mode)
         ("\\.clang-tidy\\'" . yaml-mode))

  :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
