;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c u") #'undo-fu-only-undo)

(add-hook 'help-mode-hook #'rainbow-mode)

(setq projectile-enable-caching nil)

;; (after! yasnippet
;;   (when (file-exists-p "~/.snippets")
;;     (add-to-list 'yas-snippet-dirs "~/.snippets")
;;     (yas-reload-all)))

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
  ;; Add these to the back of the list to give priority to .projectile and .git files
  (add-to-list 'projectile-project-root-files-bottom-up "compile_commands.json" t)
  (add-to-list 'projectile-project-root-files-bottom-up ".lsp-cache" t)
  (add-to-list 'projectile-project-root-files-bottom-up ".cache" t)

  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(setq ws-butler-convert-leading-tabs-or-spaces t)

(after! yasnippet
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

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (sp-local-pair 'c++-mode "/**" "*/" :actions '(navigate)) ;; Handle doxygen comment "pairs"

  (bind-key "C-M-s"
            (defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map)

  (smartparens-strict-mode) ;; Start out in strict mode
  (show-smartparens-global-mode +1)

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

(+core/add-toggle centered-cursor
                  :mode centered-cursor-mode
                  :bind '(:desc "Centered Cursor" :key "c"))

(+core/add-toggle trailing-whitespace
                  :status show-trailing-whitespace
                  :on (setq show-trailing-whitespace t)
                  :off (setq show-trailing-whitespace nil)
                  :on-message "Showing trailing whitespace"
                  :off-message "Disabled trailing whitespace"
                  :bind '(:desc "Trailing Whitespace" :key "w"))

(+core/add-toggle relative-line-numbers
                  :status (eq display-line-numbers 'relative)
                  :on (setq display-line-numbers-type 'relative)
                  :off (setq display-line-numbers-type t)
                  :on-message "Using relative line numbers"
                  :off-message "Using normal line numbers"
                  :bind '(:desc "Relative line numbers" :key "l"))

;; Default to relative line numbers
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type 'relative)))

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
