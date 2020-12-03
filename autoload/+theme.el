;;; doom.d/autoload/+theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +core/disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;###autoload
(defun +core/disable-themes (&rest _args)
  (+core/disable-all-themes))

;;;###autoload
(advice-add #'load-theme :before #'+core/disable-themes)

;;;###autoload
(add-hook #'doom-load-theme-hook (lambda () (set-face-underline 'show-paren-match t))) ;; Always underline matching parens
