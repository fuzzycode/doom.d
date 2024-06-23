;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom!
       :completion
       (corfu +icons +orderless +dabbrev)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       hl-todo
       hydra
       modeline
       nav-flash

       (popup +all +defaults)
       (treemacs +lsp)
       ophints
       (:unless (featurep :system 'windows) vc-gutter)
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       format
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons +dirvish)
       electric
       (ibuffer +icons)
       vc
       undo

       :term
       eshell
       vterm

       :tools
       debugger
       direnv
       docker
       editorconfig
       ein
       (eval +overlay)
       (lookup +dictionary +docsets)
       (lsp +peek)
       (magit +forge)
       pdf
       prodigy
       rgb
       tmux
       tree-sitter
       upload

       :checkers
       syntax
       grammar
       (spell +everywhere +flyspell
              (:cond ((executable-find "enchant-2") +enchant)
               ((executable-find "hunspell") +hunspell)
               ((executable-find "aspell") +aspell)))

       :lang
       (cc +lsp +tree-sitter)
       common-lisp
       (clojure +lsp)
       data
       emacs-lisp
       (graphql +lsp)
       (java +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       (latex +latexmk +lsp)
       markdown
       (org +dragndrop +gnuplot +hugo +jupyter +noter +pandoc +present +roam2 +pretty)
       plantuml
       (python +pyenv +lsp +tree-sitter)
       qt
       rest
       (rust +lsp +tree-sitter)
       rst
       (sh +lsp +tree-sitter +fish +powershell)
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)

       :email
       (:if (executable-find "mu") mu4e)

       :app
       (rss +org)

       :config
       (default +smartparens +bindings))
