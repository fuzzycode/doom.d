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
       company
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
       (:unless IS-WINDOWS vc-gutter)
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +ranger +icons)
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
       gist
       (lookup +dictionary +docsets)
       (lsp +peek)
       (magit (:unless IS-WINDOWS +forge))
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
       (java +lsp +tree-sitter)
       (javascript +tree-sitter)
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
       (sh +lsp +tree-sitter)
       web
       (yaml +lsp)

       :email
       (:if (executable-find "mu") mu4e)

       :app
       (rss +org)

       :config
       (default +smartparens +bindings))
