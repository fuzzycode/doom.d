;; -*- lexical-binding: t; -*-

;;DISABLED
(package! drag-stuff :disable t)

;; Stay on bleeding edge for lsp
(unpin! '(lsp-mode lsp-ui dap-modes))

;; HELM
(when (featurep! :completion helm)
  (package! helm-swoop)
  (package! helm-mode-manager)
  (package! helm-descbinds)
  (package! helm-gitignore))

;; IVY
(when (featurep! :completion ivy))

;; GIT
(package! magit-imerge)
(package! magit-tbdiff)
(package! git-commit)
(package! gitignore-mode)
(package! gitconfig-mode)
(package! gitattributes-mode)
(package! git-messenger)
(package! rigid-tabs)
(package! git-walktree)
(package! gitignore-templates)

;;ORG
(package! demo-it)
(package! idle-org-agenda)
(package! org-super-agenda)
(package! doct)
(package! org-make-toc)

;;ELISP
(package! elisp-format)
(package! eval-sexp-fu)

;; CSV
(package! csv-mode)

;; CPP MODE
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))

(package! lsp-treemacs :pin nil)

;; MAIL
(when (featurep! :email mu4e)
  (package! mu4e-maildirs-extension)
  (package! mu4e-alert))

(when (and (featurep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :recipe (:host github :repo "lordpretzel/mu4e-views")))

;; SHELL SCRIPT
;(package! sh-script :built-in)
