;;; ../Development/GitHub/dotfiles/doom.d/+latex.el -*- lexical-binding: t; -*-

;;;###package
(use-package! ebib
  :defer t
  :commands (ebib)
  :init (map! (:leader
               (:prefix "a"
                :desc "Ebib" :g "e" #'ebib))))
