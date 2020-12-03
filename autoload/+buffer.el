;;; ../Development/GitHub/dotfiles/doom.d/autoload/+buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode spacemacs-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;;;###autoload
(defun +bl/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun +bl/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; http://stackoverflow.com/a/10216338/4869
;;;###autoload
(defun +bl/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;###autoload
(defun +bl/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +bl/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

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

;;;###autoload
(defun +bl/open-junk-file ()
      (interactive)
      (let* ((fname (format-time-string open-junk-file-format (current-time)))
             (junk-dir (file-name-directory fname))
             (rel-fname (file-relative-name fname junk-dir))
             (default-directory junk-dir))

        (require 'counsel)
        (counsel-find-file rel-fname)))

;;;###autoload
(defun +bl/browse-junk-files ()
  (interactive)
  (dired (concat doom-private-dir "junk/")))

;;;###autoload
(defun +bl/special-mode-action-fn (buffer alist)
  "Marks BUFFER as special-mode and passes BUFFER and ALIST to
`+popup-display-buffer-stacked-side-window-fn'."
  (with-current-buffer buffer
    (special-mode)
    (set (make-local-variable 'window-point-insertion-type) t)
    (goto-char (point-max))
    (+popup-display-buffer-stacked-side-window-fn buffer alist)))
