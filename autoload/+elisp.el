
;;;###autoload
(defun +elisp/format-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (elisp-format-region)
    (elisp-format-buffer)))
