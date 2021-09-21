
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

