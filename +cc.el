;;; ~/.doom.d/+cc.el -*- lexical-binding: t; -*-

;; Make .h file default to cpp-mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(after! (projectile cc-mode)
  (define-key c++-mode-map (kbd "<A-tab>") #'projectile-find-other-file))

;;;###package
(use-package! ff-c-style
  :hook (c-mode-common . ff-add-c-style))
