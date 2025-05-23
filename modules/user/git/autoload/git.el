;;; user/git/autoload/git.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

;;;###autoload
(defun +bl/move-to-next-slot (&optional backward)
  "Advances to the next empty line passed a comment line.
This works because my commit message template is set up
to have a comment line as a header for each slot where
text should/could be inserted."
  (interactive "P")
  (let ((search-fn (if backward 're-search-backward 're-search-forward))
        (direction (if backward -1 1)))
    (funcall search-fn "^#.*")
    (while (not (+bl/current-line-empty-p))
      (forward-line direction))))

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

;;;###autoload
(defun +bl/smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (unless (and (not smerge-mode) (called-interactively-p 'any))
    (smerge-mode 1))
  (smerge-transient))

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
(defun +bl/magit-blame-quit-all ()
  "Ensure that all magit-blame buffers are removed when we exit the menu."
  (when (bound-and-true-p magit-blame-mode)
    (call-interactively #'magit-blame-quit))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;; Check if this buffer has magit-blame-mode active
      (when (bound-and-true-p magit-blame-mode)
        (call-interactively #'magit-blame-quit)))))

;;;###autoload
(defun +bl/transient-cleanup-h ()
  "Quit the current state that transient was used for."
  (cond
   ((bound-and-true-p magit-blame-mode) (+bl/magit-blame-quit-all))
   ((bound-and-true-p smerge-mode) (smerge-mode -1))
   ((bound-and-true-p git-timemachine-mode) (git-timemachine-quit))
   (t nil)))

;;;###autoload
(add-hook 'transient-post-exit-hook #'+bl/transient-cleanup-h)

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

The 1Password auth-source integration expects the spec to be in a specific
format that is not used nativly by ghub. This tries first to rearrange the
keys to fit 1Password and then falls back to the original function.
"
  (if-let* ((user (format "%s-%s" username package))
            (token (auth-source-pick-first-password :host user :user "password")))
      token
    (funcall orig-fun host username package nocreate forge)))
