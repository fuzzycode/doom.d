;;; ~/Development/GitHub/dotfiles/doom.d/+config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! authinfo-mode
  :defer t
  :mode "authinfo\\(\\.gpg\\)?$")

;;;###package
(use-package! subword
  :hook (prog-mode . subword-mode))
