;;; private/yaml/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :private yaml +lsp)
  (unless (executable-find "yaml-language-server")
    (warn! "lsp enabled but unable to find yaml language server. Make sure it is installed")))
