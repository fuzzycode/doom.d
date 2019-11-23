(setq user-full-name "Björnf Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "Inconsolata" :size 16)
      doom-big-font (font-spec :family "Inconsolata" :size 36))

(setq doom-leader-alt-key "M-<SPC>"
      doom-localleader-alt-key "M-<SPC> m")

(load! "+builtin")
(load! "+core")
(load! "+elisp")
(load! "+projectile")
(load! "+sh")
(load! "+ui")
(load! "+helm")
(load! "+git")

(when IS-MAC
  (load! "+osx"))

;; Allow for machine local customizations
(load! "~/.doom.local.el" "" t)

;; Use a dedicated file for custom settings
(setq custom-file (concat doom-private-dir "custom.el"))
(load! custom-file "" t)
