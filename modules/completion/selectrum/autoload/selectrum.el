;;; completion/selectrum/autoload/selectrum.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +consult/consult-line-default ()
  (interactive)
  (consult-line (doom-thing-at-point-or-region)))

;;;###autoload
(defun +consult/consult-ripgrep-project-default ()
  (interactive)
  (consult-ripgrep (doom-project-root) (doom-thing-at-point-or-region)))
