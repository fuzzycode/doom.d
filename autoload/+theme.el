;;; doom.d/autoload/+theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;###autoload
(defun +bl/disable-themes (&rest _args)
  (+bl/disable-all-themes))

;;;###autoload
(advice-add #'load-theme :before #'+bl/disable-themes)

;;;###autoload
(add-hook #'doom-load-theme-hook (lambda () (set-face-underline 'show-paren-match t))) ;; Always underline matching parens
