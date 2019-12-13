;;; ~/.doom.d/+cc.el -*- lexical-binding: t; -*-

(after! (projectile cc-mode)
  (define-key c++-mode-map (kbd "<A-tab>") #'projectile-find-other-file))

;;;###package
(use-package! ff-c-style
  :hook (c-mode-common . ff-add-c-style))
