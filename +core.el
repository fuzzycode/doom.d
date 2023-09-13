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
  (setq projectile-files-cache-expire 35)

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
        doom-modeline-checker-simple-format nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-persp-name t
        doom-modeline-vcs-max-length 45))


(after! company
  (setq company-tooltip-minimum-width 80
        company-tooltip-maximum-width 80
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(after! mu4e-alert
  (when (and IS-MAC
             (executable-find "terminal-notifier"))
    (mu4e-alert-set-default-style 'notifier))
  (setq mu4e-alert-interesting-mail-query (mapconcat #'identity '("flag:unread" "AND NOT" "flag:trashed" "AND" "maildir:/inbox") " ")))

(after! mu4e-compose
  (setq mu4e-compose-dont-reply-to-self t
              mu4e-compose-keep-self-cc nil
              mu4e-compose-complete-addresses t))

(after! mu4e
  (setq mu4e-root-maildir (expand-file-name "~/.mail")
              mu4e-drafts-folder "/drafts"
              mu4e-sent-folder   "/sent"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive"
              mu4e-update-interval 120
              mu4e-headers-auto-update t
              mu4e-headers-date-format "%Y-%m-%d %H:%M"
              mu4e-change-filenames-when-moving t
              mu4e-sent-messages-behavior 'delete
              mu4e-use-fancy-chars nil
              mu4e-headers-fields `((:human-date . 18)
                                    (:flags . 4)
                                    (:from-or-to . 20)
                                    (:subject))))

(after! vterm
  (setq vterm-shell (+bl/get-shell))

  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files))))

(after! recentf
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude #'directory-name-p) ;; Filter out all directories from the list
  (add-to-list 'recentf-exclude doom-local-dir)  ;; Remove all files in the .local folders
  (add-to-list 'recentf-exclude "\\.gz$") ;; Remove zip archives
  (add-to-list 'recentf-exclude "\\.vrb$")) ;; Remove latex intermediate files

(after! git-gutter-fringe
  (fringe-mode '(8 . 4)))  ;; Left is full size and right is half size, makes room for break-point and tilde bitmaps

(after! consult
  (advice-add #'consult--jump :around #'doom-set-jump-a))

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
                 (?j "{" . "}")
                 (?k "[" . "]")
                 (?l "<" . ">")
                 (?a "'" . "'")
                 (?s "\"" . "\""))))
    (setq-default evil-embrace-evil-surround-keys (append evil-embrace-evil-surround-keys (mapcar #'car pairs)))
    (setq-default evil-surround-pairs-alist (append evil-surround-pairs-alist pairs))))

(after! better-jumper
  (add-hook 'better-jumper-post-jump-hook #'recenter))

(after! lsp-ui
  (setq lsp-ui-peek-list-width 75)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+bl/dim-lsp-sideline))

(after! lsp-mode
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-semantic-tokens-enable t)) ; Enable semantic highlighting by default

(after! git-modes
  (add-to-list 'auto-mode-alist '("\.?gitaliases$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?gitconfig$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?\(fd\|git\)?ignore$" . gitignore-mode)))

(after! (evil code-review)
  (evil-make-overriding-map code-review-mode-map 'normal)
  (evil-set-initial-state 'code-review-comment-mode 'insert))

(after! (evil forge)
  (evil-set-initial-state 'forge-post-mode 'insert))

(after! forge
  (transient-append-suffix 'forge-dispatch "l p" '("l P" "authored PRs" forge-list-authored-pullreqs))
  (transient-append-suffix 'forge-dispatch "l i" '("l I" "authored issues" forge-list-authored-issues)))


(after! magit
  (transient-append-suffix 'magit-branch "m" '("M" "Delete Merged" +bl/delete-merged-branches))


  ;; Show 10 open topics and 10 closed ones, but only after they are toggled on
  (setq forge-topic-list-limit '(10 . -10))

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

  (when (modulep! :tools magit +forge)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpushed-to-pushremote
                            'magit-insert-modules-overview)

    (magit-add-section-hook 'magit-status-sections-hook
                            'forge-insert-requested-reviews
                            'forge-insert-pullreqs)

    (magit-add-section-hook 'magit-status-sections-hook
                            'forge-insert-authored-pullreqs
                            'forge-insert-requested-reviews)

    (magit-add-section-hook 'magit-status-sections-hook
                            'forge-insert-assigned-issues
                            'forge-insert-issues)))

(after! transient
  (transient-define-prefix smerge-transient ()
    "SMerge controlls"
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (+bl/smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (+bl/smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (+bl/smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (+bl/smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (+bl/smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (+bl/smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (+bl/smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (+bl/smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (+bl/smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (+bl/smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (+bl/smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff))))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (+bl/smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (+bl/smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (+bl/smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))

(after! (magit transient)
  (transient-define-suffix magit-submodule-update-all (args)
    "Update all submodules"
    :class 'magit--git-submodule-suffix
    :description "Update all modules git submodule update --init [--recursive]"
    (interactive (list (magit-submodule-arguments "--recursive")))
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" "--init" args)))

  (transient-append-suffix 'magit-submodule '(2 -1) '("U" magit-submodule-update-all)))

;;
;; HOOKS
;;

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'help-mode-hook #'rainbow-mode)

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type 'relative)))

(add-hook 'code-review-mode-hook (lambda () (persp-add-buffer (current-buffer))))

;;
;; SETTINGS
;;

(setq warning-minimum-level :error) ;; Only show popup on errors, warnings are still logged though

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

;;iBuffer
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 36 36 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

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
(add-to-list 'auto-mode-alist '("\\.?zshenv\\(\\.local\\)?$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zshrc\\(\\.local\\)?$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zshenv$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zprofile$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?pep8$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig.local$" . gitconfig-mode))

;; Open documentation in webkit buffer
(when (featurep 'xwidget-internal)
  (after! dash-docs
    (setq dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn))

(set-flyspell-predicate! '(c++-mode c-mode) #'+bl/cc-flyspell-predicate-p)

;; Enable advanced features without asking
(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode -1) ;; We never want that

(electric-indent-mode -1) ;; Not needed, newline and indent will take care of it

(pixel-scroll-mode t)

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
(set-popup-rule! "^\\*Code Review.*\\*$" :ignore t)
