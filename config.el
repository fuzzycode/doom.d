;; -*- lexical-binding: t; -*-

;; DOOM Settings
(setq user-full-name "Björn Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      line-spacing 1)

;Load Specific files
(load! "+bindings")
(load! "+core")
(load! "+org")
(load! "+packages")

;; Allow for machine local customization
(load! "~/.doom.local.el" "" t)
