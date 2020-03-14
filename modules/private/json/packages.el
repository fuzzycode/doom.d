
(package! json-mode)
(package! jq-mode)

(when (featurep! :completion ivy)
  (package! counsel-jq))
