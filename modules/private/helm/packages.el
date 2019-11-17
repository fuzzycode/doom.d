
(package! helm)
(package! helm-projectile)
(package! helm-swoop)
(package! helm-mode-manager)
(package! helm-descbinds)
(package! helm-swoop)

(when (featurep! +fuzzy)
  (package! helm-flx))
