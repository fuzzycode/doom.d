;;; lang/cc/config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :config (set-docsets! 'cmake-mode "CMake"))

;;;###package
(use-package! ninja-mode
  :defer t)

;;;###package
(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;;###package
(use-package! company-cmake
  :when (featurep! :completion company)
  :after cmake-mode
  :config (set-company-backend! 'cmake-mode 'company-cmake))

;;;###package
(use-package! ff-c-style
  :hook (c-mode-common . ff-add-c-style))

;;;###package
(use-package! cc-mode
  :mode ("\\.mm\\'" . objc-mode)
  :mode ("\\.h\\'" . c++-mode) ;; Almost all my h files are c++ so make that the default
  :hook (c-mode-common . rainbow-delimiters-mode)
  :config
  (set-docsets! 'c-mode "C")
  (set-docsets! 'c++-mode "C++" "Boost" "Qt"))

(when (featurep! +lsp)
  (add-hook! '(c-mode-local-vars-hook
               c++-mode-local-vars-hook
               objc-mode-local-vars-hook)
             #'lsp!))

(after! (projectile cc-mode)
  (define-key c++-mode-map (kbd "<A-tab>") #'projectile-find-other-file))
