;;; private/json/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +json/pretty-print-region-or-buffer ()
  ""
  (interactive)
  (if (region-active-p)
      (call-interactively 'json-pretty-print)
    (call-interactively 'json-pretty-print-buffer)))
