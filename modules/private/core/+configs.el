;;; private/core/+configs.el -*- lexical-binding: t; -*-

;; Setup undo-tree
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t)

(after! projectile
  (add-to-list 'projectile-project-root-files-bottom-up "compile_commands.json")
  (add-to-list 'projectile-project-root-files-bottom-up ".lsp-cache")
  (add-to-list 'projectile-project-root-files-bottom-up ".ccls-cache")

  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(setq ws-butler-convert-leading-tabs-or-spaces t)

(after! yasnippet
  (add-to-list 'yas-snippet-dirs (expand-file-name "external/" +snippets-dir))
  (add-to-list 'yas-snippet-dirs (expand-file-name "personal/" +snippets-dir))
  (yas-reload-all))

(after! smartparens
  (show-smartparens-global-mode +1))
