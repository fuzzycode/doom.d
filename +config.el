;;; ~/Development/GitHub/dotfiles/doom.d/+config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! subword
  :hook (prog-mode . subword-mode))

;;;###package
(use-package! midnight
  :defer t
  :hook (doom-first-input . midnight-mode)
  :init (setq clean-buffer-list-kill-regexps '("^\\*.*\\*$")
              clean-buffer-list-kill-never-regexps '("^\\*\\(doom\\|scratch\\|Messages\\)\\*$"
                                                     "^\\*lsp.*"
                                                     "^\\*clangd.*")))
;;;###package
(use-package! dired-x
  :ensure nil
  :defer t
  :init (map! (:leader
               (:prefix "f"
                :desc "Find file in Dired" :g "d" #'dired-jump))))
