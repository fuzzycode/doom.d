;;; ~/.doom.d/+cc.el -*- lexical-binding: t; -*-

;; Make .h file default to cpp-mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(after! (projectile cc-mode)
  (define-key c++-mode-map (kbd "<A-tab>") #'projectile-find-other-file))

;; Disable spell checking of #include statements in C/C++
(advice-add #'flyspell-generic-progmode-verify :filter-return #'+cc-no-spell-check-includes-a)

;;;###package
(use-package! ff-c-style
  :hook (c-mode-common . ff-add-c-style))
