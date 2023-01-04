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
      (user-error "There is not current branch"))))

;;;###autoload
(defun +bl/changed-files (filter base compare)
  "Return a list of absolute paths to files diffing between COMPARE and BASE, using FILTER to filter the diff.

FILTER Should be a string to be used with the --diff-filter option for git diff.
"
  (let ((files (ensure-list (split-string (cdr (doom-call-process "git" "diff" "--name-only" (format "--diff-filter=%s" filter) base compare)))))
        (root (file-name-as-directory (cdr (doom-call-process "git" "rev-parse" "--show-toplevel")))))
    (mapcar (lambda (file) (concat root file)) files)))

;;;###autoload
(defun +bl/dired-changed-files (filter base compare)
  "Shows a dired buffer with diffing files between COMPARE and BASE, using FILTER to filter the files."
  (let ((files (+bl/changed-files filter base compare))
        (name "Dired Changed Files"))
    (if files
        (dired (cons name files))
      (message "No Changed files found"))))

;;;###autoload
(defun +bl/dired-added-files (base compare)
  "Lists all added files between COMPARE and BASE."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))
                     (magit-read-branch "Compare" (magit-get-current-branch))))
  (+bl/dired-changed-files "A" base compare))

;;;###autoload
(defun +bl/dired-modified-files (base compare)
  "Lists all modified files between COMPARE and BASE."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))
                     (magit-read-branch "Compare" (magit-get-current-branch))))
  (+bl/dired-changed-files "M" base compare))
