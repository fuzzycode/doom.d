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
(use-package! json-snatcher
  :defer t
  :init (map! :localleader :map json-mode-map :desc "Print Path" :g "p" #'jsons-print-path))

;;;###package
(use-package! jq-mode
  :defer t
  :mode "\\.jq$"
  :init (with-eval-after-load 'json-mode
          (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively)
          (map! :localleader :map json-mode-map :desc "Interactive jq" :g "j" #'jq-interactively)))

;;;###package
(use-package! counsel-jq
  :when (featurep! :completion ivy)
  :after (counsel json-mode)
  :init (map! :localleader :map json-mode-map :desc "Counsel jq" :g "c" #'counsel-jq))
