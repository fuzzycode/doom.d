
;;;###autoload
(defun +core/inflection-cycle-dwim ()
  "switching by major-mode"
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    (string-inflection-ruby-style-cycle))))

;;;###autoload
(defun +core/indent-region-or-buffer ()
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
(defun +core/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun +core/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; http://stackoverflow.com/a/10216338/4869
;;;###autoload
(defun +core/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;###autoload
(defun +core/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +core/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;;###autoload
(defun +core/switch-to-message-buffer ()
  "Switch to the `*Messages*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))
