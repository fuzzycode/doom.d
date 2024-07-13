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
  (setq auto-revert-check-vc-info t
        doom-modeline-height 35 ;; default is 25 and that is a bit too small for me
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-indent-info t
        doom-modeline-check-simple-format nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-persp-name t
        doom-modeline-vcs-max-length 60))


(after! company
  (setq company-tooltip-minimum-width 80
        company-tooltip-maximum-width 80
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

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

(after! magit
  (transient-append-suffix 'magit-branch "m" '("M" "Delete merged" +bl/delete-merged-branches))
  (transient-append-suffix 'magit-log "-n" '("-M" "Ignore merges" "--no-merges"))

  ;; Show 10 open topics and 10 closed ones, but only after they are toggled on
  (setq forge-topic-list-limit '(10 . -10))

  ;; Show images in commit buffers
  (setq magit-revision-show-gravatars t)

  (magit-wip-after-save-mode)

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

  (setq magit-submodule-list-columns '(("Path" 50 magit-modulelist-column-path nil)
                                       ("Version" 35 magit-repolist-column-version
                                        ((:sort magit-repolist-version<)))
                                       ("Branch" 35 magit-repolist-column-branch nil)
                                       ("B<U" 3 magit-repolist-column-unpulled-from-upstream
                                        ((:right-align t)
                                         (:sort <)))
                                       ("B>U" 3 magit-repolist-column-unpushed-to-upstream
                                        ((:right-align t)
                                         (:sort <)))
                                       ("B<P" 3 magit-repolist-column-unpulled-from-pushremote
                                        ((:right-align t)
                                         (:sort <)))
                                       ("B>P" 3 magit-repolist-column-unpushed-to-pushremote
                                        ((:right-align t)
                                         (:sort <)))
                                       ("B" 3 magit-repolist-column-branches
                                        ((:right-align t)
                                         (:sort <)))
                                       ("S" 3 magit-repolist-column-stashes
                                        ((:right-align t)
                                         (:sort <)))))

  (when (modulep! :tools magit +forge)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpushed-to-pushremote
                            'magit-insert-modules-overview)))

(after! transient
  (transient-bind-q-to-quit)

  (defvar +bl/transient--exit-function nil
    "Temporarily store the cleanup function to use when exiting a transient")
  
  (transient-define-prefix text-zoom-transient ()
    "Text Size Controlls"
    :transient-suffix 'transient--do-stay
    [["Size"
      ("j" "Increase" doom/increase-font-size)
      ("k" "Decrease" doom/decrease-font-size)
      ("0" "Reset" doom/reset-font-size :transient transient--do-quit-one)]
     ["Toggle"
      ("B" "Big Font" doom-big-font-mode :transient transient--do-quit-one)]])

  (transient-define-prefix git-timemachine-transient ()
    "Git Time Machine"
    :transient-suffix 'transient--do-stay
    [["Show Revision"
      ("c" "Current Revision" git-timemachine-show-current-revision )
      ("g" "Nth Revision" git-timemachine-show-nth-revision)
      ("p" "Previous Revision" git-timemachine-show-previous-revision)
      ("n" "Next Revision" git-timemachine-show-next-revision)
      ("N" "Previous Revision" git-timemachine-show-previous-revision)]
     ["Copy Revision"
      ("y" "Abbreviated Revision" git-timemachine-kill-abbreviated-revision)
      ("Y" "Full Revision" git-timemachine-kill-revision)]]
    (interactive)
    (let ((cleanup (lambda ()
                     (when git-timemachine-mode
                       (call-interactively 'git-timemachine-quit))
                     (remove-hook 'transient-exit-hook +bl/transient--exit-function)
                     (setq +bl/transient--exit-function nil))))
      (setq +bl/transient--exit-function cleanup)
      (add-hook 'transient-exit-hook cleanup))
    (transient-setup 'git-timemachine-transient))

  (transient-define-prefix magit-blame-transient ()
    "Git Blame"
    :transient-suffix 'transient--do-quit-one
    [["Blame"
      ("b" "Blame Further" magit-blame-addition :transient transient--do-stay)]
     ["Copy"
      ("y" "Revision" magit-blame-copy-hash)]]
     (interactive)
     (let ((cleanup (lambda ()
                      (when magit-blame-mode
                        (call-interactively 'magit-blame-quit))
                      (remove-hook 'transient-exit-hook +bl/transient--exit-function)
                      (setq +bl/transient--exit-function nil))))
       (setq +bl/transient--exit-function cleanup)
       (add-hook 'transient-exit-hook cleanup))
     (transient-setup 'magit-blame-transient))

    (transient-define-prefix smerge-transient ()
      "SMerge controlls"
      :transient-suffix 'transient--do-stay
      [["Move"
        ("n" "Next" smerge-next)
        ("N" "Next(All Files)" smerge-vc-next-conflict)
        ("p" "Previous" smerge-prev)]
       ["Keep"
        ("b" "Base" smerge-keep-base)
        ("u" "Upper(Mine)" smerge-keep-upper)
        ("l" "Lower(Other)" smerge-keep-lower)
        ("a" "All" smerge-keep-all)
        ("RET" "Current" smerge-keep-current)]
       ["Diff"
        ("<" "Base/Upper" smerge-diff-base-upper)
        ("=" "Upper/Lower" smerge-diff-upper-lower)
        (">" "Base/Lower" smerge-diff-base-lower)
        ("F" "Refine" smerge-refine)
        ("E" "Ediff" smerge-ediff)]
       ["Other"
        ("c" "Combine" smerge-combine-with-next)
        ("C" "Auto Combine" smerge-auto-combine)
        ("r" "Resolve" smerge-resolve)
        ("R" "Resolve All" smerge-resolve-all)
        ("k" "Kill Current" smerge-kill-current)]]
      (interactive)
      (let ((cleanup (lambda ()
                       (when smerge-mode
                         (smerge-mode -1))
                       (remove-hook 'transient-exit-hook +bl/transient--exit-function)
                       (setq +bl/transient--exit-function nil))))
        (setq +bl/transient--exit-function cleanup)
        (add-hook 'transient-exit-hook cleanup))
      (transient-setup 'smerge-transient))
)

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

(add-hook 'text-mode-hook (lambda () (setq-local completion-at-point-functions
                                                 (list (cape-capf-super #'cape-dabbrev #'ispell-completion-at-point #'yasnippet-capf)))))


(add-hook! prog-mode
  (defun +bl/add-comment-spelling-capf-h ()
    (add-hook 'completion-at-point-functions (cape-capf-super (cape-capf-inside-comment #'ispell-completion-at-point) #'cape-dabbrev #'yasnippet-capf) 10 t)))

(add-hook! prog-mode
  (defun +bl/add-string-spelling-capf-h ()
    (add-hook 'completion-at-point-functions (cape-capf-super (cape-capf-inside-string #'ispell-completion-at-point) #'cape-dabbrev #'yasnippet-capf) 10 t)))
;;
;; SETTINGS
;;

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

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
(when (featurep :system 'macos)
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
