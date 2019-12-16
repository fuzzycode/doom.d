;;; private/json/config.el -*- lexical-binding: t; -*-

(map! (:localleader
        :map json-mode-map
        (:prefix ("=" . "format")
          :desc "Format Region Or Buffer" :g "=" #'+json/pretty-print-region-or-buffer
          :desc "Format Buffer" :g "b" #'json-pretty-print-buffer
          :desc "Format Region" :g "r" #'json-pretty-print)))

;;;###package
(use-package! json-mode
  :mode "\\.json\\'")

;;;###package
(use-package json-snatcher
  :defer t)

;;;###package
(use-package! json-navigator
  :defer t)
