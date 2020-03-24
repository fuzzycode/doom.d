;; DOOM Settings
(setq user-full-name "Björn Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      line-spacing 1.5)

(setq doom-leader-alt-key "M-<SPC>"
      doom-localleader-alt-key "M-<SPC> m")

(map! (:leader (:prefix ("m" . "Mode Leader"))))

;; Load Specific files
(load! "+builtin")
(load! "+core")
(load! "+elisp")
(load! "+projectile")
(load! "+sh")
(load! "+ui")

(when (featurep! :completion helm)
       (load! "+helm"))

(when (featurep! :completion ivy)
  (load! "+ivy"))

(load! "+git")
(load! "+org")
(load! "+lsp")
(load! "+cc")
(load! "+python")
(load! "+bindings")
(load! "+config")

(when IS-MAC
  (load! "+osx"))

;; Allow for machine local customizations
(load! "~/.doom.local.el" "" t)

;; Use a dedicated file for custom settings
(setq custom-file (concat doom-private-dir "custom.el"))
(load! custom-file "" t)
