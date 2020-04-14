
;;;###autoload
(defun +magit/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))


;;;###autoload
(defun +magit/move-to-next-slot ()
  "Advances to the next empty line passed a comment line. This works because my commit message template is set up
to have a comment line as a header for each slot where text should/could be inserted."
  (interactive)
  (re-search-forward "^#.*")
  (while (not (+magit/current-line-empty-p))
    (forward-line 1)))


;;;###autoload
(add-hook 'find-file-hook (lambda ()
                             (when (string-suffix-p "COMMIT_EDITMSG" buffer-file-name)
                               (goto-char (point-min))
                               (+magit/move-to-next-slot))))

;;;###autoload
(defvar +magit/mainline-branch "master"
  "The default branch for a project")

;;;###autoload
(defun +magit/diff-file-against-mainline ()
  ""
  (interactive)
  (when buffer-file-name
    (magit-diff-range +org/mainline-branch nil (list buffer-file-name))))

;;;###autoload
(defun +magit/diff-worktree-against-mainline ()
  ""
  (interactive)
  (magit-diff-range +magit/mainline-branch))
