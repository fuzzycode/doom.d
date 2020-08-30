;; -*- no-byte-compile: t; -*-
;;; private/folding/packages.el

(package! origami)

(when (featurep! :tools lsp)
  (package! lsp-origami))
