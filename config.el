;; -*- lexical-binding: t; -*-

;; DOOM Settings
(setq user-full-name "Björn Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      line-spacing 1)

;; Setup all standard prefixes
(map! (:leader
       (:prefix ("a" . "applications"))
       (:prefix ("b" . "buffer/bookmarks"))
       (:prefix ("c" . "code"))
       (:prefix ("d" . "documentation"))
       (:prefix ("e" . "errors"))
       (:prefix ("E" . "ediff"))
       (:prefix ("f" . "files"))
       (:prefix ("g" . "git"))
       (:prefix ("h" . "help"))
       (:prefix ("i" . "insert"))
       (:prefix ("j" . "jump/join"))
       (:prefix ("k" . "pairs"))
       (:prefix ("K" . "keyboard macros"))
       (:prefix ("l" . "lines"))
       (:prefix ("m" . "mode leader"))
       (:prefix ("n" . "narrow"))
       (:prefix ("o" . "org"))
       (:prefix ("p" . "project"))
       (:prefix ("q" . "quit/reload"))
       (:prefix ("r" . "resume"))
       (:prefix ("R" . "rectangles"))
       (:prefix ("s" . "search"))
       (:prefix ("S" . "spelling"))
       (:prefix ("t" . "toggle"))
       (:prefix ("w" . "windows"))
       (:prefix ("x" . "text"))))

;Load Specific files
(load! "+bindings")
(load! "+core")
(load! "+ivy")
(load! "+org")
(load! "+packages")

;; Allow for machine local customizations
(load! "~/.doom.local.el" "" t)
