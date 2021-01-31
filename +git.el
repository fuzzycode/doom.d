;; -*- lexical-binding: t; -*-

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

(map!
 (:leader
  (:prefix "g"
   :desc "Magit File Dispatch" :ng "." #'magit-file-dispatch
   :desc "Magit Status" :ng "s" #'magit-status
   :desc "Magit Dispatch" :ng "m" #'magit-dispatch
   (:when (featurep! :tools magit +forge)
    :desc "Forge Dispatch" :ng "F" #'forge-dispatch)
   :desc "Stage File" :ng "S" #'magit-stage-file
   :desc "Unstage File" :ng "U" #'magit-unstage-file
   :desc "Time Machine" :ng "t" #'+magit/timemachine-hydra/body
   :desc "Git Blame" :ng "b" #'+magit/blame-hydra/body
   :desc "Magit Refresh" :ng "r" #'magit-refresh
   :desc "Magit Refresh All" :ng "R" #'magit-refresh-all
   (:prefix ("d" . "diff")
    :desc "Diff dwim" :ng "d" #'magit-diff-dwim
    :desc "File...Mainline" :ng "f" #'+magit/diff-file-against-mainline
    :desc "Worktree...Mainline" :ng "w" #'+magit/diff-worktree-against-mainline
    :desc "Diff Paths" :ng "p" #'magit-diff-paths
    :desc "Diff Range" :ng "r" #'magit-diff-range)
   (:prefix ("f" . "file")
    :desc "Log File" :ng "l" #'magit-log-buffer-file
    :desc "Diff" :ng "d" #'magit-diff
    :desc "Magit Find File" :ng "f" #'magit-find-file)
   (:prefix ("B" . "browse")
    (:when (featurep! :emacs vc)
     :desc "Browse region or line" :ng "." #'+vc/git-browse-region-or-line)
    :desc "Browse remote" :ng "r" #'forge-browse-remote
    :desc "Browse commit" :ng "c" #'forge-browse-commit
    :desc "Browse an issue" :ng "i" #'forge-browse-issue
    :desc "Browse a pull request" :ng "p" #'forge-browse-pullreq
    :desc "Browse issues" :ng "I" #'forge-browse-issues
    :desc "Browse pull requests" :ng "P" #'forge-browse-pullreqs)
   (:prefix ("c" . "create")
    :desc "Initialize repo" :ng "r"   #'magit-init
    :desc "Clone repo" :ng "R" #'magit-clone
    :desc "Commit" :ng "c" #'magit-commit-create
    :desc "Fixup" :ng "f" #'magit-commit-fixup
    :desc "Issue" :ng "i" #'forge-create-issue
    :desc "Pull request" :ng "p" #'forge-create-pullreq)
   (:when (featurep! :tools gist)
    (:prefix ("g" . "gist")
     :desc "Gist Buffer" :ng "b" #'gist-buffer
     :desc "Gist Buffer (private)" :ng "B" #'gist-buffer-private
     :desc "Gist Region" :ng "r" #'gist-region
     :desc "Gist Region (private)" :ng "R" #'gist-region-private
     :desc "Gist dwim" :ng "d" #'gist-region-or-buffer
     :desc "Gist dwim (private)" :ng "D" #'gist-region-or-buffer-private))
   (:prefix ("l" . "link")
    :desc "Git Link" :ng "l" #'git-link
    :desc "Git Link Commit" :ng "c" #'git-link-commit)
   (:prefix ("L" . "list")
    (:when (featurep! :tools gist)
     :desc "List gists" :ng "g" #'+gist:list)
    :desc "List repositories" :ng "r" #'magit-list-repositories
    :desc "List submodules" :ng "s" #'magit-list-submodules
    :desc "List issues":ng "i" #'forge-list-issues
    :desc "List pull requests" :ng "p" #'forge-list-pullreqs
    :desc "List notifications" :ng "n" #'forge-list-notifications))))

;; Better scrolling in magit buffers
;;;###package magit
(map! :after magit
      (:map magit-status-mode-map
       :vng "C-j" #'magit-section-forward-sibling
       :vng "C-k" #'magit-section-backward-sibling))

;;;###package
(use-package! git-commit
  :defer t
  :init
  (add-hook 'git-commit-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
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
  (map! (:leader (:prefix "g" :desc "Git Messenger" :ng "M" #'git-messenger:popup-message))))

;;;###package
(use-package! git-walktree
  :defer t
  :commands (git-walktree)
  :init (map! (:leader
                (:prefix "g"
                  :desc "Walk Tree" :ng "w" #'git-walktree))))
;;;###package
(use-package! gitignore-templates
  :defer t
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                        :desc "Insert Ignore Template" :ng "i" #'gitignore-templates-insert
                        :desc "New Ignore File" :ng "I" #'gitignore-templates-new-file)))
