;;; user/git/config.el -*- lexical-binding: t; -*-

(use-package! diff-dired
  :disabled t
  :defer t
  :commands (diff-dired-list-added diff-dired-listmodified diff-dired-list-changed)
  :when (modulep! :tools magit)
  :init (map! (:leader (:prefix "g"
                                (:prefix "l"
                                 :desc "List Added Files" "a" #'diff-dired-list-added
                                 :desc "List Modified Files" "m" #'diff-dired-list-modified
                                 :desc "List Changed Files" "c" #'diff-dired-list-changed)))))

(use-package! ssh-agency
  :when (featurep :system 'windows)
  :when (modulep! :tools magit))

(use-package! git-link
  :defer t
  :init (map! :leader
              (:prefix "g"
               :desc "Git Link" "w" #'git-link)))

(use-package! git-commit
  :defer t
  :when (modulep! :tools magit)
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (when (modulep! :editor evil)
    (add-hook 'git-commit-mode-hook #'evil-insert-state))
  :bind (:map git-commit-mode-map
              ([tab] . #'+bl/move-to-next-slot)))

(use-package! magit-imerge
  :when (modulep! :tools magit)
  :after magit
  :config
  (map! (:map magit-status-mode-map
              "i" #'magit-imerge
              "#" #'magit-gitignore))
  (transient-insert-suffix 'magit-dispatch "I" '("i" "iMerge" magit-imerge))
  (transient-insert-suffix 'magit-dispatch "!" '("#" "Ignore" magit-gitignore))
  (transient-append-suffix 'magit-merge "n" '("g" "iMerge" magit-imerge)))

(use-package! magit-lfs
  :when (modulep! :tools magit)
  :after magit)

(use-package! gitignore-templates
  :defer t
  :when (modulep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                       :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                       :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))

(use-package pr-review
  :after forge)

;; (add-hook 'code-review-mode-hook (lambda () (persp-add-buffer (current-buffer))))

;; (set-popup-rule! "^\\*Code Review.*\\*$" :ignore t)

(after! git-gutter-fringe
  (fringe-mode '(8 . 4)))  ;; Left is full size and right is half size, makes room for break-point and tilde bitmaps

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
    ;; Hack to make ghub--token try to find it in 1password first
    (advice-add 'ghub--token :around #'+bl/ghub--token-a)

    (magit-add-section-hook 'magit-status-sections-hook
                            #'+bl/forge-insert-reviews-todo
                            #'forge-insert-pullreqs)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules-unpushed-to-pushremote
                            'magit-insert-modules-overview)))

(after! transient
  (transient-bind-q-to-quit)

  (defvar-local +bl/transient--exit-function nil
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
      ("y" "Abbreviated Revision" git-timemachine-kill-abbreviated-revision :transient transient--do-quit-one)
      ("Y" "Full Revision" git-timemachine-kill-revision :transient transient--do-quit-one)]]
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
    :transient-suffix 'transient--do-stay
    [["Blame"
      ("b" "Blame Further" magit-blame-addition)]
     ["Move"
      ("n" "Next Chunk" magit-blame-next-chunk)
      ("N" "Next Chunk(Same Commit)" magit-blame-next-chunk-same-commit)
      ("p" "Previous Chunk" magit-blame-previous-chunk)
      ("P" "Previous Chunk(Same Commit)" magit-blame-previous-chunk-same-commit)]
     ["Other"
      ("c" "Cycle Style" magit-blame-cycle-style)
      ("y" "Copy Revision" magit-blame-copy-hash :transient transient--do-quit-one)
      ("v" "Visit Blob" magit-blame-visit-file :transient transient--do-quit-one)]]
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
        ("E" "Ediff" smerge-ediff :transient transient--do-quit-one)]
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
