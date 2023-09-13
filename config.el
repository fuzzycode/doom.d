;; -*- lexical-binding: t; -*-

;; DOOM Settings
(setq user-full-name "Björn Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)
      ;; doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light)
      line-spacing 1)

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Load Specific files
(load! "+core")
(load! "+bindings")
(load! "+org")
(load! "+packages")

;; Allow for machine local customization
(load! "~/.doom.local.el" "" t)
