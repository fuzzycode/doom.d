;;; ../Development/GitHub/dotfiles/doom.d/+latex.el -*- lexical-binding: t; -*-

(use-package! ebib
  :defer t
  :init (map! (:leader
               (:prefix "a"
                :desc "Ebib" :g "e" #'ebib))))
