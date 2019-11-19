(setq user-full-name "Björnf Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "Inconsolata" :size 16)
      doom-big-font (font-spec :family "Inconsolata" :size 36))

(setq doom-leader-alt-key "M-<SPC>"
      doom-localleader-alt-key "M-<SPC> m")

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq bidi-display-reordering nil)
(setq bidi-paragraph-direction 'left-to-right)

(when IS-MAC
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-right-command-modifier nil
        mac-right-option-modifier nil))


(load! "+core")
(load! "+elisp")
(load! "+projectile")

;; Allow for machine local customizations
(load! "~/.doom.local.el" "" t)

;; Use a dedicated file for custom settings
(setq custom-file (concat doom-private-dir "custom.el"))
(load! custom-file "" t)
