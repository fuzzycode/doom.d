;;; ~/.doom.d/autoload/+ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ivy/ivy-search-project-default ()
  (interactive)
  (+ivy/project-search nil (regexp-quote (ivy-thing-at-point))))
