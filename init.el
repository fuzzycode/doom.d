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
       (ivy +prescient +childframe)

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping

       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)
       treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       file-templates    ; auto-snippets for empty files
       ;; fold              ; (nigh) universal code folding
       ;; (format +onsave)  ; automated prettiness
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +ranger +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       vterm             ; another terminals in Emacs

       :tools
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :checkers
       syntax
       (spell +aspell)

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp)                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;go                ; the hipster dialect
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;; (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +latexmk)            ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +hugo            ; use Emacs for hugo blogging
        +ipython         ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        +journal
        +present)        ; using org-mode for presentations
       ;;perl              ; write code no one else can comprehend
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +pyenv +lsp)            ; beautiful is better than ugly
       qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       rst
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       web               ; the tubes

       :email
       mu4e
       
       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       rss        ; emacs as an RSS reader
       regex
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs for writers (fiction, notes, papers, etc.)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +smartparens)

       :private
       core
       (yaml +lsp)
       json
       cmake
       )
