;; -*- lexical-binding: t; -*-

;;DISABLED
(package! drag-stuff :disable t)

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
(package! gitattributes-mode)
(package! git-messenger)
(package! rigid-tabs)
(package! git-walktree)
(package! gitignore-templates)

;;ORG
(package! demo-it)
(package! idle-org-agenda)
(package! org-super-agenda)
(package! doct :recipe (:host github :repo "progfolio/doct" :branch "master"))
(package! org-make-toc)

;;ELISP
(package! elisp-format)
(package! eval-sexp-fu)

;; CSV
(package! csv-mode)

;; CPP MODE
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))

;; Authinfo files
(package! authinfo-mode :recipe (:host github :repo "fuzzycode/authinfo-mode"))

;; SHELL SCRIPT
;(package! sh-script :built-in)
