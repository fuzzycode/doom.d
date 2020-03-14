
(package! json-mode)
(package! json-snatcher)
(package! jq-mode)

(when (featurep! :completion ivy)
  (package! counsel-jq))
