;; -*- no-byte-compile: t; -*-
;;; private/debugging/packages.el


(when (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))
  (package! dap-mode))
