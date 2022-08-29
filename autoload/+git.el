;; -*- lexical-binding: t; -*-
;;;###if (modulep! :tools magit)


;;;###autoload
(defun +bl/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))


;;;###autoload
(defun +bl/move-to-next-slot ()
  "Advances to the next empty line passed a comment line. This works because my commit message template is set up
to have a comment line as a header for each slot where text should/could be inserted."
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
                               (when IS-WINDOWS
                                 (+bl/delete-carrage-returns))
                               (goto-char (point-min))
                               (+bl/move-to-next-slot))))

