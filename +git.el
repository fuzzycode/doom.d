
(after! magit

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

  (setq magit-save-repository-buffers nil
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
                          'magit-insert-unpulled-from-upstream))

(map! (:leader
        (:prefix "g"
          :desc "Magit Status" :g "s" #'magit-status
          :desc "Magit Dispatch" :g "m" #'magit-dispatch
          :desc "Forge Dispatch" :g "F" #'forge-dispatch
          :desc "Stage File" :g "S" #'magit-stage-file
          :desc "Unstage File" :g "U" #'magit-unstage-file
          :desc "Time Machine" :g "T" #'+magit/timemachine-hydra/body
          :desc "Git Blame" :g "b" #'+magit/blame-hydra/body
          :desc "Magit Refresh" :g "r" #'magit-refresh
          :desc "Magit Refresh All" :g "R" #'magit-refresh-all
          (:prefix ("d" . "diff")
            :desc "Diff dwim" :g "d" #'magit-diff-dwim
            :desc "File...Mainline" :g "f" #'+magit/diff-file-against-mainline
            :desc "Worktree...Mainline" :g "w" #'+magit/diff-worktree-against-mainline
            :desc "Diff Paths" :g "p" #'magit-diff-paths
            :desc "Diff Range" :g "r" #'magit-diff-range)
          (:prefix ("f" . "file")
            :desc "Log File" :g "l" #'magit-log-buffer-file
            :desc "Diff" :g "d" #'magit-diff
            :desc "Magit Find File" :g "f" #'magit-find-file)
          (:prefix ("B" . "browse")
            (:when (featurep! :emacs vc)
              :desc "Browse region or line" :g "." #'+vc/git-browse-region-or-line)
            :desc "Browse remote" :g "r" #'forge-browse-remote
            :desc "Browse commit" :g "c" #'forge-browse-commit
            :desc "Browse an issue" :g "i" #'forge-browse-issue
            :desc "Browse a pull request" :g "p" #'forge-browse-pullreq
            :desc "Browse issues" :g "I" #'forge-browse-issues
            :desc "Browse pull requests" :g "P" #'forge-browse-pullreqs)
          (:prefix ("c" . "create")
            :desc "Initialize repo" :g "r"   #'magit-init
            :desc "Clone repo" :g "R" #'magit-clone
            :desc "Commit" :g "c" #'magit-commit-create
            :desc "Fixup" :g "f" #'magit-commit-fixup
            :desc "Issue" :g "i" #'forge-create-issue
            :desc "Pull request" :g "p" #'forge-create-pullreq)
          (:when (featurep! :tools gist)
            (:prefix ("g" . "gist")
              :desc "Gist Buffer" :g "b" #'gist-buffer
              :desc "Gist Buffer (private)" :g "B" #'gist-buffer-private
              :desc "Gist Region" :g "r" #'gist-region
              :desc "Gist Region (private)" :g "R" #'gist-region-private
              :desc "Gist dwim" :g "d" #'gist-region-or-buffer
              :desc "Gist dwim (private)" :g "D" #'gist-region-or-buffer-private))
          (:prefix ("l" . "link")
            :desc "Git Link" :g "l" #'git-link
            :desc "Git Link Commit" :g "c" #'git-link-commit)
          (:prefix ("L" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists" :g "g" #'+gist:list)
            :desc "List repositories" :g "r" #'magit-list-repositories
            :desc "List submodules" :g "s" #'magit-list-submodules
            :desc "List issues":g "i" #'forge-list-issues
            :desc "List pull requests" :g "p" #'forge-list-pullreqs
            :desc "List notifications" :g "n" #'forge-list-notifications))))

(map! :map git-commit-mode-map
      [tab] #'+magit/goto-first-empty-line)

(after! git-commit
  (add-hook 'git-commit-mode-hook 'fci-mode))

(after! git-config
  (add-to-list 'auto-mode-alist '("\.?gitaliases$" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\.?gitconfig$" . gitconfig-mode)))

(after! gitignore
  (add-to-list 'auto-mode-alist '("\.?gitignore$" . gitignore-mode)))


(after! magit-todos
  (setq magit-todos-recursive t
        magit-todos-require-colon nil)
  (custom-set-variables
   '(magit-todos-keywords (list "TODO(Björn Larsson)" "HACK" "FIXME" "XXX" "???")))
  (shut-up (magit-todos-mode)))

;;;###package
(use-package! gitattributes-mode)

;;;###package
(use-package magit-imerge
  :after magit)

;;;###package
(use-package git-messenger
  :defer t
  :commands git-messenger:popup-close
  :init (map! (:leader (:prefix "g" :desc "Git Messenger" :g "M" #'git-messenger:popup-message)))
  :bind (:map git-messenger-map
          ([escape] . git-messenger:popup-close)))

;;;###package
(use-package! git-walktree
  :defer t
  :init (map! (:leader
                (:prefix "g"
                  :desc "Walk Tree" :g "w" #'git-walktree))))
;;;###package
(use-package! gitignore-templates
  :commands (gitignore-templates-insert gitignore-templates-new-file)
  :init (map! :leader (:prefix "g"
                        :desc "Insert Ignore Template" :g "i" #'gitignore-templates-insert
                        :desc "New Ignore File" :g "I" #'gitignore-templates-new-file)))
