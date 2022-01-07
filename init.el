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
       vc-gutter
       vi-tilde-fringe
       (window-select +numbers)
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
       (magit +forge)
       pdf
       prodigy
       rgb
       tmux
       upload

       :checkers
       syntax
       grammar
       (spell +everywhere +flyspell
              (:cond ((executable-find "enchant-2") +enchant)
               ((executable-find "hunspell") +hunspell)
               ((executable-find "aspell") +aspell)))

       :lang
       (cc +lsp)
       common-lisp
       (clojure +lsp)
       data
       emacs-lisp
       (java +lsp)
       javascript
       (json +lsp)
       (latex +latexmk +lsp)
       markdown
       (org +dragndrop +gnuplot +hugo +jupyter +noter +pandoc +present +pretty +roam2)
       plantuml
       (python +pyenv +lsp)
       qt
       rest
       (rust +lsp)
       rst
       (sh +lsp)
       web
       (yaml +lsp)

       :email
       (:if (executable-find "mu") mu4e)

       :app
       (rss +org)

       :config
       (default +smartparens +bindings))
