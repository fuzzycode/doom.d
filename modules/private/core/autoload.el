
;;;###autoload
(defun +core/top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
