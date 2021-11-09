;; -*- lexical-binding: t; -*-

;;
;; PACKAGES
;;
(after! flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                     ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                     ("Col" 3 nil :right-align t)
                                     ("Level" 8 flycheck-error-list-entry-level-<)
                                     ("ID" 35 t)
                                     (#("Message (Checker)" 9 16
                                        (face flycheck-error-list-checker-name))
                                      0 t)]))

(after! projectile
  (setq projectile-enable-caching nil)

  (put 'projectile-project-name 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'file-directory-p)

  ;; Add this to the back of the list to give priority to .projectile and .git files
  (add-to-list 'projectile-project-root-files-bottom-up "compile_commands.json" t)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(after! warnings
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))


(after! smartparens
  (setq sp-escape-wrapped-region t
        sp-escape-quotes-after-insert t)

  (sp-local-pair 'c++-mode "/**" "*/" :actions '(navigate)) ;; Handle doxygen comment "pairs"

  (smartparens-strict-mode) ;; Start out in strict mode
  (show-smartparens-global-mode +1))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-indent-info t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-checker-simple-format nil
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-version t
        doom-modeline-mu4e t
        doom-modeline-vcs-max-length 45))


(after! company
  (setq company-tooltip-minimum-width 80
        company-tooltip-maximum-width 80
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; TODO(Björn Larsson): Remove the need for a hard coded path parts
(after! recentf
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude #'directory-name-p) ;; Filter out all directories from the list
  (add-to-list 'recentf-exclude (concat ".*?" "\\.emacs\\.d/\\.local" ".*"))  ;; Remove all files in the .local folders
  (add-to-list 'recentf-exclude "\\.vrb$")) ;; Remove latex intermediate files

(after! git-gutter-fringe
  (fringe-mode '(8 . 4)))  ;; Left is full size and right is half size, makes room for break-point and tilde bitmaps

(after! evil-snipe
  (setq evil-snipe-spillover-scope 'whole-visible))

(after! evil-goggles
  (setq evil-goggles-duration 0.2))

(after! evil
  (setq evil-want-fine-undo t
      evil-kill-on-visual-paste nil))

(after! evil-surround
  (let ((pairs '((?g "$" . "$")
                 (?h "(" . ")")
                 (?j "[" . "]")
                 (?k "{" . "}")
                 (?l "<" . ">")
                 (?a "'" . "'")
                 (?s "\"" . "\""))))
    (setq-default evil-embrace-evil-surround-keys (append evil-embrace-evil-surround-keys (mapcar #'car pairs)))
    (setq-default evil-surround-pairs-alist (append evil-surround-pairs-alist pairs))))

(after! lsp-ui
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+bl/dim-lsp-sideline))

(after! lsp-mode
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq lsp-semantic-tokens-enable t)) ; Enable semantic highlighting by default

(after! magit-gitflow
  ;; Add a binding for ignore commands that is missing from evil bindings
  (transient-insert-suffix 'magit-dispatch "%" '("=" "Ignore" magit-gitignore)))

(after! git-modes
  (add-to-list 'auto-mode-alist '("\.?gitaliases$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?gitconfig$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\\.?\\(fd\\|git\\)?ignore$" . gitignore-mode)))

(after! magit
  ;; Show 100 open topics and 10 closed ones, but only after they are toggled on
  (setq forge-topic-list-limit '(100 . -10))

  ;; Show images in commit buffers
  (setq magit-revision-show-gravatars t)

  (magit-wip-after-save-mode)
  (transient-bind-q-to-quit)

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; A colour highlight is enough, no need to warn me again
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks))

  (setq magit-save-repository-buffers 'dontask
        magit-section-visibility-indicator nil
        magit-wip-merge-branch t
        magit-refs-primary-column-width '(16 . 92)
        magit-process-finish-apply-ansi-colors t
        transient-enable-popup-navigation t)

  (setq magit-repolist-columns '(("Name" 35 magit-repolist-column-ident nil)
                                 ("Status" 7 magit-repolist-column-flag nil)
                                 ("Version" 35 magit-repolist-column-version nil)
                                 ("B<U" 4 magit-repolist-column-unpulled-from-upstream
                                  ((:right-align t)
                                   (:help-echo "Upstream changes not in branch")))
                                 ("B>U" 4 magit-repolist-column-unpushed-to-upstream
                                  ((:right-align t)
                                   (:help-echo "Local changes not in upstream")))
                                 ("Path" 99 magit-repolist-column-path nil)))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)

  (when (featurep! :tools magit +forge)
    (magit-add-section-hook 'magit-status-sections-hook
                            'forge-insert-assigned-pullreqs
                            'magit-insert-modules-overview)

    (magit-add-section-hook 'magit-status-sections-hook
                            'forge-insert-requested-reviews
                            'forge-insert-assigned-pullreqs)))
;;
;; SETTINGS
;;

(setq plantuml-indent-level 2)

(setq ws-butler-convert-leading-tabs-or-spaces t)

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq ispell-dictionary "en_US")

;; Show the location of the sym-link and not the actual file
(setq find-file-visit-truename nil
      vc-follow-symlinks nil)

(setq x-stretch-cursor t
      ns-pop-up-frames nil
      create-lockfiles nil
      require-final-newline t
      use-dialog-box nil
      use-file-dialog nil)

;; iSeach
(setq isearch-lax-whitespace t
      search-whitespace-regexp ".*?"
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t)

;; Don't confirm closing emacs with running processes
(setq confirm-kill-processes nil)

;; Backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Compilation
(setq compilation-auto-jump-to-first-error nil
      compilation-skip-threshold 2)

;; Add extensionless file modes
(add-to-list 'auto-mode-alist '("\\.zshenv.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc.local$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?pep8$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.?zshenv$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zprofile$" . sh-mode))

;; Open documentation in webkit buffer
(when (featurep 'xwidget-internal)
  (after! dash-docs
    (setq dash-docs-browser-func '+lookup-xwidget-webkit-open-url-fn))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn))

(set-flyspell-predicate! '(c++-mode c-mode) #'+bl/cc-flyspell-predicate-p)

;; Enable advanced features without asking
(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode -1) ;; We never want that

(electric-indent-mode -1) ;; Not needed, newline and indent will take care of it

;; mac specifics
(when IS-MAC
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-right-command-modifier nil
        mac-right-option-modifier nil))

;; File templates
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)

;;
;; POPUPS
;;

(set-popup-rule! "^\\*Shell Command Output\\*$" :side 'bottom :height 40 :select nil :actions '(+bl/special-mode-action-fn))
(set-popup-rule! "^\\*YASnippet Tables\\*$" :quit 'other :side 'bottom :height 0.5 :select t)
(set-popup-rule! "^\\*Org Export Dispatcher\\*$" :side 'right :width 0.6)
(set-popup-rule! "^CAPTURE-.*\\.org$" :size 0.5 :quit nil :select t :autosave t)
(set-popup-rule! "^\\*xwidget" :vslot -11 :size 0.55 :select nil)
