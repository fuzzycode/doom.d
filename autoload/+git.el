
;;;###autoload
(defun +magit/goto-first-empty-line (&optional from-top)
  "Finds the first empty line and moves point to it. If from-top is true the search will start from the top of the buffer."
  (interactive)
  (when from-top
    (goto-char (point-min)))

  (forward-char)
  (re-search-forward "^$"))

;;;###autoload
(add-hook 'find-file-hook (lambda ()
                             (when (string-suffix-p "COMMIT_EDITMSG" buffer-file-name)
                               (+magit/goto-first-empty-line t))))
