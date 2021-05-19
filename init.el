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
       company          ; the ultimate code completion backend
       (ivy +prescient +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping

       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)
       file-templates    ; auto-snippets for empty files
       fold
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +ranger +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree
       undo

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       vterm             ; another terminals in Emacs

       :tools
       debugger
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       magit    ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       prodigy
       rgb               ; creating color strings
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :checkers
       syntax
       grammar
       (spell +everywhere +flyspell +enchant)

       :lang
       (cc +lsp)                ; C/C++/Obj-C madness
       common-lisp       ; if you've seen one lisp, you've seen them all
       emacs-lisp        ; drown in parentheses
       (java +lsp)                ; the poster child for carpal tunnel syndrome
       javascript          ; all(hope(abandon(ye(who(enter(here))))))
       (latex +latexmk +lsp)            ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org
        +brain
        +dragndrop
        +gnuplot
        +hugo
        +jupyter
        +pandoc
        +present)
       plantuml          ; diagrams for confusing people more
       (python +pyenv +lsp)            ; beautiful is better than ugly
       qt                ; the 'cutest' gui framework ever
       rest              ; Emacs as a REST client
       (rust +lsp)
       rst
       (sh +lsp)           ; she sells {ba,z,fi}sh shells on the C xor
       web               ; the tubes
       (yaml +lsp)
       json

       :email
       mu4e

       :app
       (rss +org)        ; emacs as an RSS reader

       :config
       (default +smartparens))
