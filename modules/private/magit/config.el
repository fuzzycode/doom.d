
(use-package! magit
  :defer t
  :commands (magit-status
             magit-dispatch-popup
             magit-stage-file
             magit-unstage-file)
  :init
  (setq transient-levels-file  (concat doom-etc-dir "transient/levels")
         transient-values-file  (concat doom-etc-dir "transient/values")
         transient-history-file (concat doom-etc-dir "transient/history")
         magit-save-repository-buffers nil
         git-commit-summary-max-length 50
         git-commit-turn-on-flyspell t
         git-commit-turn-on-auto-fill t
         magit-section-visibility-indicator nil
         magit-wip-merge-branch t)

  (setq transient-enable-popup-navigation t)

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  (map! (:leader
          (:prefix ("g" . "git")
            :desc "Magit Status" :g "s" 'magit-status
            :desc "Dispatch" :g "m" 'magit-dispatch
            :desc "Stage File" :g "S" 'magit-stage-file
            :desc "Unstage File" :g "U" 'magit-unstage-file)))
  :config
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn)
  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  (magit-wip-after-save-mode)
  (transient-bind-q-to-quit)

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions
    (defun +magit-buffer-p (buf)
      (with-current-buffer buf
        (and (derived-mode-p 'magit-mode)
             (not (eq major-mode 'magit-process-mode))))))

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (add-to-list 'magit-no-confirm 'trash)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))

(use-package! gitattributes-mode
  :after magit)

(use-package! gitconfig-mode
  :after magit)

(use-package! gitignore-mode
  :after magit)

(use-package! git-commit
  :after magit)

(use-package magit-imerge
  :after magit)

(use-package helm-gitignore
    :defer t
    :init
    (map! (:leader
            (:prefix "g"
              :desc "Git Ignore" :g "I" 'helm-gitignore))))

(use-package git-messenger
  :defer t
  :commands git-messenger:popup-close
  :init (map! (:leader (:prefix "g" :desc "Git Messenger" :g "M" 'git-messenger:popup-message)))
  :bind (:map git-messenger-map
          ([escape] . git-messenger:popup-close)))
