;;; user/git/autoload/git.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))


;;;###autoload
(defun +bl/move-to-next-slot ()
  "Advances to the next empty line passed a comment line.
This works because my commit message template is set up
to have a comment line as a header for each slot where
text should/could be inserted."
  (interactive)
  (re-search-forward "^#.*")
  (while (not (+bl/current-line-empty-p))
    (forward-line 1)))

;;;###autoload
(defun +bl/delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;;;###autoload
(add-hook 'find-file-hook (lambda ()
                            (when (string-suffix-p "COMMIT_EDITMSG" buffer-file-name)
                              (when (featurep :system 'windows)
                                (+bl/delete-carrage-returns))
                              (goto-char (point-min))
                              (when (looking-at-p "^[[:space:]]*#.*$")
                                (+bl/move-to-next-slot)))))

;;;###autoload
(defun +bl/magit-add-current-branch-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (require 'magit)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is no current branch"))))


;;;###autoload
(defun +bl/delete-merged-branches (target)
  "Remove all branches merged into TARGET."
  (interactive (list (magit-read-branch "Target")))
  (let ((merged (delete target (ensure-list (magit-list-merged-branches target)))))
    (if merged
        (when (yes-or-no-p (format "Really delete %s ?" (mapconcat 'identity merged ", ")))
          (magit-branch-delete merged))
      (message "No merged branches found"))))


(defun +bl/get-commit-at-point ()
  "Tries to get the commit at point."
  (cond
   ((or (eq major-mode 'magit-status-mode) (eq major-mode 'magit-log-mode))
    (call-interactively 'magit-copy-section-value))

   ((or (eq major-mode 'magit-commit-mode) (eq major-mode 'magit-revision-mode))
    (call-interactively 'magit-copy-buffer-revision))))

;;;###autoload
(defun +bl/kill-url-to-commit-at-point ()
  "Copies the URL to the current commit at point."
  (interactive)
  (let ((commit (+bl/get-commit-at-point)))
    (if commit
        (progn
          (require 'browse-at-remote)
          (kill-new (message "%s" (browse-at-remote--commit-url commit))))
      (message "No commit found at point"))))

;;;###autoload
(defun +bl/maybe-show-smerge-transient-h ()
  (when smerge-mode
    (call-interactively 'smerge-transient)))

;;;###autoload
(add-hook 'magit-diff-visit-file-hook #'+bl/maybe-show-smerge-transient-h)

;;;###autoload
(defun +bl/maybe-show-time-machine-transient-h ()
  (when git-timemachine-mode
    (call-interactively 'git-timemachine-transient)))

;;;###autoload
(add-hook 'git-timemachine-mode-hook #'+bl/maybe-show-time-machine-transient-h)

;;;###autoload
(defun +bl/maybe-show-blame-transient-h ()
  (when magit-blame-mode
    (call-interactively 'magit-blame-transient)))

;;;###autoload
(add-hook 'magit-blame-mode-hook #'+bl/maybe-show-blame-transient-h)

;;;###autoload
(defadvice! +bl/smerge-recenter-after-move-a (&rest _)
  :after #'smerge-next
  :after #'smerge-prev
  (save-excursion
    (beginning-of-line)
    (when (looking-at smerge-begin-re)
      (recenter))))

;;;###autoload
(defun +bl/ediff-copy-both-to-c ()
  "Add both A and B to the resulting C buffer. Accepting both changes."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

;;;###autoload
(defun +bl/git-link-dispatch ()
  (interactive)
  (require 'git-link-transient)
  (git-link-dispatch))

;;;###autoload
(defun +bl/add-c-to-ediff-mode-map-h () (define-key ediff-mode-map (kbd "c") #'+bl/ediff-copy-both-to-c))

;;;###autoload
(add-hook 'ediff-keymap-setup-hook #'+bl/add-c-to-ediff-mode-map-h)

;;;###autoload
(defun +bl/pr-review-from-forge-maybe ()
  "Try to start a PR review using URL of forge target at point."
  (interactive)
  (if-let* ((target (forge--browse-target))
            (url (forge-get-url target)))
      (pr-review url)
    (user-error "No PR to review at point")))

;;;###autoload
(defun +bl/git-repo-sync ()
  "Sync the current git repository with the remote.
This will fetch all changes from origin and pull all forge topics"
  (interactive)
  (magit-fetch-all nil)
  (forge-pull))

;;;###autoload
(defun +bl/forge-insert-reviews-todo ()
  (forge-insert-topics 'todo-pullreq "Prioritized Reviews"
    (lambda (repo)
      (and-let* ((me (ghub--username repo)))
        (forge--topics-spec :type 'pullreq :active t :reviewer me)))))

;;;###autoload
(defun +bl/ghub--token-a (orig-fun host username package &optional nocreate forge)
  "A hack to try harder to find the token in 1Password first.

The 1Password auth-source integration expects the spec to be in a specific format
that is not used nativly by ghub. This tries first to rearrange the keys to fit 1Password
and then falls back to the original function.
"
  (if-let* ((user (format "%s-%s" username package))
            (token (auth-source-pick-first-password :host user :user "password")))
      token
    (funcall orig-fun host username package nocreate forge)))
