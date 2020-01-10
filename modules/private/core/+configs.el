;;; private/core/+configs.el -*- lexical-binding: t; -*-

;; Setup undo-tree
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t)

(after! projectile
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(setq ws-butler-convert-leading-tabs-or-spaces t)

(use-package flyspell-correct-ivy
  :when (featurep! :completion ivy)
  :bind (:map flyspell-mode-map ("C-;" . #'flyspell-correct-wrapper))
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy)
  (map! (:leader
          (:prefix ("S" . "spelling")
            (:when (featurep! :tools flyspell)
              :desc "Correct Next" :g "n" #'flyspell-correct-next
              :desc "Correct Previous" :g "p" #'flyspell-correct-previous
              :desc "Correct At Point" :g "c" #'flyspell-correct-at-point
              :desc "Correct DWIM" :g "s" #'flyspell-correct-wrapper)))))
