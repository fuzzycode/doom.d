
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
(package! magit-todos)
(package! rigid-tabs)
(package! git-walktree)

;;ORG
(package! demo-it)
(package! idle-org-agenda)
(package! org-projectile)
(package! org-super-agenda)
(package! org-sticky-header)
(package! org-bullets)

;;ELISP
(package! elisp-format)

;; CPP MODE
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))

;; SHELL SCRIPT
;(package! sh-script :built-in)
