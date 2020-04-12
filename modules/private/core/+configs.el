;;; private/core/+configs.el -*- lexical-binding: t; -*-

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

  (sp-local-pair 'c++-mode "/**" "*/" :actions '(navigate)) ;; Handle doxygen comment "pairs"

  (show-smartparens-global-mode +1))

;; Add extensionless file modes
(add-to-list 'auto-mode-alist '("\\.zshenv.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.fdignore$" . gitignore-mode))


(after! smartparens

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (smartparens-strict-mode) ;; Start out in strict mode

  (+core/add-toggle sp-strict-mode
                    :mode smartparens-strict-mode
                    :bind '(:desc "Smartparens Strict Mode" :key "s")))

;; Add toggles
(+core/add-toggle word-wrap
                  :mode +word-wrap-mode
                  :bind '(:desc "Word Wrap" :key "w"))

(+core/add-toggle big-font
                  :mode doom-big-font-mode
                  :bind '(:desc "Big Font" :key "b"))

(+core/add-toggle flycheck
                  :mode flycheck-mode
                  :bind '(:desc "Flycheck" :key "f"))

(+core/add-toggle read-only
                  :mode read-only-mode
                  :bind '(:desc "Read Only" :key "r"))
