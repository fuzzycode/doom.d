;; -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("\\.clang-format\\'" . yaml-mode)
         ("\\.clang-tidy\\'" . yaml-mode))
  :hook ((yaml-mode . lsp))
  :bind (:map yaml-mode-map
          ( "\C-m" . #'newline-and-indent)))
