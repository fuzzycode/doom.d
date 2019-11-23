
;;;###autoload
(defun +elisp/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;;;###autoload
(defun +elisp/indent-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (indent-region)
    (+elisp/indent-buffer)))
