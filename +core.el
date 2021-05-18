;; -*- lexical-binding: t; -*-

(setq plantuml-indent-level 2)

(after! flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                     ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                     ("Col" 3 nil :right-align t)
                                     ("Level" 8 flycheck-error-list-entry-level-<)
                                     ("ID" 35 t)
                                     (#("Message (Checker)" 9 16
                                        (face flycheck-error-list-checker-name))
                                      0 t)]))

(setq ibuffer-formats '((mark modified read-only locked " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

(after! projectile
  (setq projectile-enable-caching nil)

  (put 'projectile-project-name 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'file-directory-p)

  ;; Add this to the back of the list to give priority to .projectile and .git files
  (add-to-list 'projectile-project-root-files-bottom-up "compile_commands.json" t)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(setq ws-butler-convert-leading-tabs-or-spaces t)

(after! yasnippet
  (when (file-exists-p "~/.snippets")
    (add-to-list 'yas-snippet-dirs "~/.snippets")

  (add-to-list 'yas-snippet-dirs (expand-file-name "external/" +snippets-dir))
  (add-to-list 'yas-snippet-dirs (expand-file-name "personal/" +snippets-dir))
  (yas-reload-all))

;; Add extensionless file modes
(add-to-list 'auto-mode-alist '("\\.zshenv.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.fdignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.clangd$". yaml-mode))

(after! smartparens
  (setq sp-escape-wrapped-region t
        sp-escape-quotes-after-insert t)

  (sp-local-pair 'c++-mode "/**" "*/" :actions '(navigate)) ;; Handle doxygen comment "pairs"

  (smartparens-strict-mode) ;; Start out in strict mode
  (show-smartparens-global-mode +1))

;; Open documentation in webkit buffer
(when (featurep 'xwidget-internal)
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-indent-info t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-checker-simple-format nil
        doom-modeline-persp-name t
        doom-modeline-persp-name-icon t
        doom-modeline-lsp t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-version t
        doom-modeline-mu4e t
        doom-modeline-vcs-max-length 45))

(set-flyspell-predicate! '(c++-mode c-mode) #'cc-flyspell-predicate-p)

;;;###package evil-snipe
(after! evil-snipe
  (setq evil-snipe-spillover-scope 'whole-visible))

(after! lsp-ui
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline))

(after! lsp-mode
  (setq lsp-enable-semantic-highlighting t)) ; Enable semantic highlighting by default

;; This is a temporary workaround for
;; https://github.com/hlissner/doom-emacs/issues/4894
(add-hook! lsp-mode
  (defalias '+lookup/references 'lsp-find-references))

(when (and (featurep! :tools lsp) (featurep! :completion ivy))
  (setq lsp-ivy-show-symbol-filename nil)) ; remove the file path from workspace symbols
