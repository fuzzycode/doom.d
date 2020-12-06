;; -*- lexical-binding: t; -*-

(after! magit

  ;; Show 100 open topics and 10 closed ones, but only after they are toggled on
  (setq forge-topic-list-limit '(100 . -10))

  ;; Show images in commit buffers
  (setq magit-revision-show-gravatars t)

  (magit-wip-after-save-mode)
  (transient-bind-q-to-quit)

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

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

(map! (:leader
        (:prefix "g"
          :desc "Magit Status" :nvg "s" #'magit-status
          :desc "Magit Dispatch" :nvg "m" #'magit-dispatch
          (:when (featurep! :tools magit +forge)
           :desc "Forge Dispatch" :nvg "F" #'forge-dispatch)
          :desc "Stage File" :nvg "S" #'magit-stage-file
          :desc "Unstage File" :nvg "U" #'magit-unstage-file
          :desc "Time Machine" :nvg "T" #'+magit/timemachine-hydra/body
          :desc "Git Blame" :nvg "b" #'+magit/blame-hydra/body
          :desc "Magit Refresh" :nvg "r" #'magit-refresh
          :desc "Magit Refresh All" :nvg "R" #'magit-refresh-all
          (:prefix ("d" . "diff")
            :desc "Diff dwim" :nvg "d" #'magit-diff-dwim
            :desc "File...Mainline" :nvg "f" #'+magit/diff-file-against-mainline
            :desc "Worktree...Mainline" :nvg "w" #'+magit/diff-worktree-against-mainline
            :desc "Diff Paths" :nvg "p" #'magit-diff-paths
            :desc "Diff Range" :nvg "r" #'magit-diff-range)
          (:prefix ("f" . "file")
            :desc "Log File" :nvg "l" #'magit-log-buffer-file
            :desc "Diff" :nvg "d" #'magit-diff
            :desc "Magit Find File" :nvg "f" #'magit-find-file)
          (:prefix ("B" . "browse")
            (:when (featurep! :emacs vc)
              :desc "Browse region or line" :nvg "." #'+vc/git-browse-region-or-line)
            :desc "Browse remote" :nvg "r" #'forge-browse-remote
            :desc "Browse commit" :nvg "c" #'forge-browse-commit
            :desc "Browse an issue" :nvg "i" #'forge-browse-issue
            :desc "Browse a pull request" :nvg "p" #'forge-browse-pullreq
            :desc "Browse issues" :nvg "I" #'forge-browse-issues
            :desc "Browse pull requests" :nvg "P" #'forge-browse-pullreqs)
          (:prefix ("c" . "create")
            :desc "Initialize repo" :nvg "r"   #'magit-init
            :desc "Clone repo" :nvg "R" #'magit-clone
            :desc "Commit" :nvg "c" #'magit-commit-create
            :desc "Fixup" :nvg "f" #'magit-commit-fixup
            :desc "Issue" :nvg "i" #'forge-create-issue
            :desc "Pull request" :nvg "p" #'forge-create-pullreq)
          (:when (featurep! :tools gist)
            (:prefix ("g" . "gist")
              :desc "Gist Buffer" :nvg "b" #'gist-buffer
              :desc "Gist Buffer (private)" :nvg "B" #'gist-buffer-private
              :desc "Gist Region" :nvg "r" #'gist-region
              :desc "Gist Region (private)" :nvg "R" #'gist-region-private
              :desc "Gist dwim" :nvg "d" #'gist-region-or-buffer
              :desc "Gist dwim (private)" :nvg "D" #'gist-region-or-buffer-private))
          (:prefix ("l" . "link")
            :desc "Git Link" :nvg "l" #'git-link
            :desc "Git Link Commit" :nvg "c" #'git-link-commit)
          (:prefix ("L" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists" :nvg "g" #'+gist:list)
            :desc "List repositories" :nvg "r" #'magit-list-repositories
            :desc "List submodules" :nvg "s" #'magit-list-submodules
            :desc "List issues":nvg "i" #'forge-list-issues
            :desc "List pull requests" :nvg "p" #'forge-list-pullreqs
            :desc "List notifications" :nvg "n" #'forge-list-notifications))))

;;;###package
(use-package! git-commit
  :defer t
  :init (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  :bind (:map git-commit-mode-map
        ([tab] . #'+magit/move-to-next-slot)))

;;;###package
(use-package! gitconfig-mode
  :mode ("\.?gitaliases$" . gitconfig-mode)
  :mode ("\.?gitconfig$" . gitconfig-mode))

;;;###package
(use-package! gitignore-mode
  :mode ("\.?gitignore$" . gitignore-mode))

;;;###package
(use-package! gitattributes-mode
  :defer t)

;;;###package
(use-package magit-imerge
  :defer t
  :after magit)

;;;###package
(use-package! magit-tbdiff
  :defer t
  :after magit)

;;;###package
(use-package git-messenger
  :defer t
  :commands (git-messenger:popup-message)
  :init (setq  git-messenger:use-magit-popup t
               git-messenger:show-detail t)
  (map! (:leader (:prefix "g" :desc "Git Messenger" :nvg "M" #'git-messenger:popup-message))))

;;;###package
(use-package! git-walktree
  :defer t
  :commands (git-walktree)
  :init (map! (:leader
                (:prefix "g"
                  :desc "Walk Tree" :nvg "w" #'git-walktree))))
;;;###package
(use-package! gitignore-templates
  :defer t
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                        :desc "Insert Ignore Template" :nvg "i" #'gitignore-templates-insert
                        :desc "New Ignore File" :nvg "I" #'gitignore-templates-new-file)))
