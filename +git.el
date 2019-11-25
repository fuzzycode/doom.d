
(after! magit

  (magit-wip-after-save-mode)
  (transient-bind-q-to-quit)

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  (setq magit-save-repository-buffers nil
        magit-section-visibility-indicator nil
        magit-wip-merge-branch t
        magit-refs-primary-column-width '(16 . 52)
        magit-process-finish-apply-ansi-colors t
        transient-enable-popup-navigation t))

(map! (:leader
        (:prefix ("g" . "git")
          :desc "Magit Status" :g "s" #'magit-status
          :desc "Magit Dispatch" :g "m" #'magit-dispatch
          :desc "Forge Dispatch" :g "F" #'forge-dispatch
          :desc "Log File" :g "l" #'magit-log-buffer-file
          :desc "Stage File" :g "S" #'magit-stage-file
          :desc "Unstage File" :g "U" #'magit-unstage-file
          :desc "Time Machine" :g "T" #'+magit/timemachine-hydra/body
          :desc "Git Blame" :g "B" #'+magit/blame-hydra/body
          :desc "Magit Refresh" :g "r" #'magit-refresh
          :desc "Magit Refresh All" :g "R" #'magit-refresh-all
          (:prefix ("b" . "browse")
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
  (add-to-list 'auto-mode-alist '("\.?gitconfig$" . gitconfig-mode)))

(after! gitignore
  (add-to-list 'auto-mode-alist '("\.?gitignore$" . gitignore-mode)))


;;;###package
(use-package! gitattributes-mode)

;;;###package
(use-package magit-imerge
  :after magit)

;;;###package
(use-package git-messenger
  :defer t
  :commands git-messenger:popup-close
  :init (map! (:leader (:prefix "g" :desc "Git Messenger" :g "M" 'git-messenger:popup-message)))
  :bind (:map git-messenger-map
          ([escape] . git-messenger:popup-close)))
