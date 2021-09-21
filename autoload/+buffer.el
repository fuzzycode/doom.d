;;; ../Development/GitHub/dotfiles/doom.d/autoload/+buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/switch-to-message-buffer ()
  "Switch to the `*Messages*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

;;;###autoload
(defun +bl/uniquify-lines-buffer ()
  "Uniwuify lines in whole buffer"
  (interactive)
  (save-excursion
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (delete-duplicate-lines)))

;;;###autoload
(defun +bl/uniquify-lines-dwim ()
  "Uniquify lines in region if active and in buffer if not."
  (interactive)
  (if (region-active-p)
      (delete-duplicate-lines)
    (+bl/uniquify-lines-buffer)))

(defun +bl/junk-directory ()
  "Returns the directory where junk files are stored"
  (let ((fname (format-time-string open-junk-file-format (current-time))))
    (file-name-directory fname)))

;;;###autoload
(defun +bl/browse-junk-files ()
  (interactive)
  (dired (+bl/junk-directory)))

;;;###autoload
(defun +bl/special-mode-action-fn (buffer alist)
  "Marks BUFFER as special-mode and passes BUFFER and ALIST to
`+popup-display-buffer-stacked-side-window-fn'."
  (with-current-buffer buffer
    (special-mode)
    (set (make-local-variable 'window-point-insertion-type) t)
    (goto-char (point-max))
    (+popup-display-buffer-stacked-side-window-fn buffer alist)))
