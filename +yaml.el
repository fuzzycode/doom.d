;;; ~/Development/GitHub/dotfiles/doom.d/+yaml.el -*- lexical-binding: t; -*-


(use-package! yaml-mode
  :mode (("\\.clang-format\\'" . yaml-mode)
         ("\\.clang-tidy\\'" . yaml-mode))
  :bind (:map yaml-mode-map
          ( "\C-m" . #'newline-and-indent)))
