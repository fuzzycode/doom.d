(setq user-full-name "Björnf Larsson"
      user-mail-address "develop@bjornlarsson.net")

(setq doom-font (font-spec :family "Inconsolata" :size 16)
      doom-big-font (font-spec :family "Inconsolata" :size 36))

(setq doom-leader-alt-key "M-<SPC>"
      doom-localleader-alt-key "M-<SPC> m")

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
