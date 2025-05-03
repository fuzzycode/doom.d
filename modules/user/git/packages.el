;; -*- no-byte-compile: t; lexical-binding: t;-*-
;;; user/git/packages.el

(package! code-review :disable t) ;; Use emacs-pr-review instead

(when (featurep :system 'windows)
  (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))

(package! pr-review :pin "7c2ce9deafe5158b4b0fe7c1e70104d64727c15e")
(package! git-link :pin "8d0f98cf36f6b9c31285329b054ae77f9a3d9b33")
;; (package! diff-dired :recipe (:host github :repo "fuzzycode/diff-dired"))
(package! magit-imerge :pin "05532a364f52b0dfed445256cb052592234183a5")
(package! magit-lfs :pin "cd9f46e1840270be27e2c2d9dcf036ff0781f66d")
(package! rigid-tabs :pin "c05d4c692fbda3859fb764b673c4c52b7d6cd3e5")
(package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099")
