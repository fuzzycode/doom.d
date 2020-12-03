
;;;###autoload
(defun +text/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;;;###autoload
(defun +text/indent-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (indent-region)
    (+elisp/indent-buffer)))

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
(defun +core/disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

;;;###autoload
(add-hook 'minibuffer-setup-hook #'+core/disable-key-chord-mode)
