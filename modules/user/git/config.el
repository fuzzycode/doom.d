;;; user/git/config.el -*- lexical-binding: t; -*-

(map! :leader (:prefix "g" (:prefix ("p" . "PR"))))

;;
;; Packages added by this module
;;

(use-package! ssh-agency
  :when (featurep :system 'windows)
  :when (modulep! :tools magit))

(use-package! git-link
  :defer t
  :init
  (map! :leader
        (:prefix "g"
         :desc "Git Link" "y" #'git-link
         :desc "Git Link Commit" "Y" #'git-link-commit)))

(use-package! git-commit
  :defer t
  :when (modulep! :tools magit)
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (when (modulep! :editor evil)
    (add-hook 'git-commit-mode-hook #'evil-insert-state))
  :bind (:map git-commit-mode-map
              ("S-<tab>" . (lambda ()(interactive)(+bl/move-to-next-slot t)))
              ([tab] . #'+bl/move-to-next-slot)))

(use-package! rigid-tabs
  :after magit
  :when (modulep! :tools magit)
  :config
  (add-hook 'diff-mode-hook #'rigid-tabs-diff-align)
  (add-hook 'magit-refresh-buffer-hook #'rigid-tabs-diff-align))

(use-package! magit-imerge
  :when (modulep! :tools magit)
  :after magit
  :config
  (map! (:map magit-status-mode-map
              "i" #'magit-imerge
              "#" #'magit-gitignore))
  (transient-insert-suffix 'magit-dispatch "I" '("i" "iMerge" magit-imerge))
  (transient-insert-suffix 'magit-dispatch "!" '("#" "Ignore" magit-gitignore))
  (transient-append-suffix 'magit-merge "m" '("i" "iMerge" magit-imerge)))

(use-package! magit-lfs
  :when (modulep! :tools magit)
  :after magit)

(use-package! gitignore-templates
  :defer t
  :when (modulep! :tools magit)
  :commands (gitignore-templates-insert gitignore-templates-new-file))

(use-package pr-review
  :defer t
  :when (modulep! :tools magit)
  :init (map! :leader (:prefix "gp"
                       :desc "Search" "s" #'pr-review-search
                       :desc "Mine" "p" (cmd! (pr-review-search "is:pr is:open author:@me"))
                       :desc "Todo" "t" (cmd! (pr-review-search "is:pr is:open review-requested:@me -review:approved -is:draft"))
                       :desc "Review" "r" #'pr-review))
  (evil-ex-define-cmd "prr" #'pr-review)
  (evil-ex-define-cmd "prs" #'pr-review-search)
  (evil-ex-define-cmd "prn" #'pr-review-notification)

  (after! evil
    (evil-set-initial-state 'pr-review-input-mode 'insert))

  :bind (:map magit-status-mode-map
              ("C-c C-r" . #'+bl/pr-review-from-forge-maybe)))

;;
;; Configuration of related packages added by other modules
;;

(after! git-gutter-fringe
  (fringe-mode '(8 . 4)))  ;; Left is full size and right is half size, makes room for break-point and tilde bitmaps

(after! git-modes
  (add-to-list 'auto-mode-alist '("\.?gitaliases$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?gitconfig$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?\(fd\|git\)?ignore$" . gitignore-mode)))


(after! (evil forge)
  (evil-set-initial-state 'forge-post-mode 'insert))

(after! forge
  ;; Show 10 open topics and 10 closed ones, but only after they are toggled on
  (setq forge-topic-list-limit '(10 . -10)))

(after! magit
  ;;   (transient-append-suffix 'magit-branch "m" '("M" "Delete merged" +bl/delete-merged-branches))
  (transient-append-suffix 'magit-log "-n" '("-M" "Ignore merges" "--no-merges"))

  (transient-replace-suffix 'magit-dispatch "O" '("X" "Reset" magit-reset))

  ;; Hack to make ghub--token try to find it in 1password first
  (advice-add 'ghub--token :around #'+bl/ghub--token-a)

  ;; Show images in commit buffers
  (setq magit-revision-show-gravatars t)

  (magit-wip-after-save-mode)

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks))

  (setq magit-save-repository-buffers 'dontask
        magit-section-visibility-indicator nil
        magit-wip-merge-branch t
        magit-refs-primary-column-width '(16 . 92)
        magit-process-apply-ansi-colors t)

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

  ;;     (magit-add-section-hook 'magit-status-sections-hook
  ;;                             #'+bl/forge-insert-reviews-todo
  ;;                             #'forge-insert-pullreqs)
  ;;
  ;;Tune what information is shown in the status buffer
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-modules-overview))

(after! transient
  (transient-bind-q-to-quit)

  (setq transient-enable-popup-navigation t)

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
    :transient-non-suffix t
    [["Show Revision"
      ("c" "Current Revision" git-timemachine-show-current-revision )
      ("g" "Nth Revision" git-timemachine-show-nth-revision)
      ("p" "Previous Revision" git-timemachine-show-previous-revision)
      ("n" "Next Revision" git-timemachine-show-next-revision)
      ("N" "Previous Revision" git-timemachine-show-previous-revision)]
     ["Copy Revision"
      ("y" "Abbreviated Revision" git-timemachine-kill-abbreviated-revision :transient transient--do-quit-one)
      ("Y" "Full Revision" git-timemachine-kill-revision :transient transient--do-quit-one)]
     ["Copy Link"
      ("l" "Copy Link" git-link :transient transient--do-quit-one :inapt-if-not (lambda () (fboundp 'git-link)))]])

  (transient-define-prefix magit-blame-transient ()
    "Git Blame"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix t
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
      ("v" "Visit Blob" magit-blame-visit-file :transient transient--do-quit-one)]])

  (transient-define-prefix smerge-transient ()
    "SMerge controlls"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix t
    [["Move"
      ("n" "Next" smerge-next)
      ("N" "Next(All Files)" smerge-vc-next-conflict)
      ("p" "Previous" smerge-prev)]
     ["Keep"
      ("b" "Base" smerge-keep-base :inapt-if-not (lambda () (smerge-check 2)))
      ("u" "Upper(Mine)" smerge-keep-upper :inapt-if-not (lambda () (smerge-check 1)))
      ("l" "Lower(Other)" smerge-keep-lower :inapt-if-not (lambda () (smerge-check 3)))
      ("a" "All" smerge-keep-all :inapt-if-not (lambda () (smerge-check 1)))
      ("RET" "Current" smerge-keep-current :inapt-if-not (lambda () (ignore-errors (and (smerge-check 1) (> (smerge-get-current) 0)))))]
     ["Diff"
      ("<" "Base/Upper" smerge-diff-base-upper :inapt-if-not (lambda () (smerge-check 2)))
      ("=" "Upper/Lower" smerge-diff-upper-lower :inapt-if-not (lambda () (smerge-check 1)))
      (">" "Base/Lower" smerge-diff-base-lower :inapt-if-not (lambda () (smerge-check 2)))
      ("F" "Refine" smerge-refine :inapt-if-not (lambda () (smerge-check 1)))
      ("E" "Ediff" smerge-ediff :transient transient--do-quit-one :inapt-if-not (lambda () (smerge-check 1)))]
     ["Other"
      ("c" "Combine" smerge-combine-with-next :inapt-if-not (lambda () (smerge-check 1)))
      ("C" "Auto Combine" smerge-auto-combine :inapt-if-not (lambda () (smerge-check 1)))
      ("r" "Resolve" smerge-resolve :inapt-if-not (lambda () (smerge-check 1)))
      ("R" "Resolve All" smerge-resolve-all)
      ("k" "Kill Current" smerge-kill-current)]]))

(after! ediff
  ;; Make quitting out of ediff easier
  (setq ediff-autostore-merges t)

  (defadvice! +bl/ediff-quit-noprompt (orig-fn &rest args)
    "Quit ediff without prompting for confirmation."
    :around #'ediff-quit
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (apply orig-fn args))))
