
(use-package! paradox
  :defer t
  :init (map! (:leader (:prefix "a"
                         :desc "Paradox List Packages" :g "l" 'paradox-list-packages)))
  (setq paradox-column-width-package 30
        paradox-column-width-version 15))

(use-package! beginend
  :defer 5
  :diminish beginend-global-mode
  :config (beginend-global-mode 1))

(use-package! visual-regexp-steroids
  :after visual-regexp)

(use-package! visual-regexp
  :bind (([remap replace-regexp] . 'vr/replace)
         ([remap query-replace-regexp] . 'vr/query-replace)
         ("C-c r" . 'vr/replace)
         ("C-c q" . 'vr/query-replace)
         ("C-c m" . 'vr/mc-mark)))

(use-package! comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

(use-package! smart-backspace
  :bind ([remap backward-delete-char-untabify] . smart-backspace))

(use-package! proced
   :defer t
   :hook (proced-mode . (lambda () (proced-toggle-auto-update +1)))
   :init (map! (:leader (:prefix ("a" . "applications")
                          :desc "Proced" :g "P" 'proced)))
   :config
   (setq proced-auto-update-interval 1)
   (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))

(use-package! mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)))

(use-package! winum
  :init (setq winum-auto-assign-0-to-minibuffer nil
              winum-auto-setup-mode-line nil
              winum-ignored-buffers '(" *which-key*"))
  :config (winum-mode)
  :bind (:map winum-keymap
          ("M-1" . 'winum-select-window-1)
          ("M-2" . 'winum-select-window-2)
          ("M-3" . 'winum-select-window-3)
          ("M-4" . 'winum-select-window-4)
          ("M-5" . 'winum-select-window-5)
          ("M-6" . 'winum-select-window-6)
          ("M-7" . 'winum-select-window-7)
          ("M-8" . 'winum-select-window-8)
          ("M-9" . 'winum-select-window-9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extras
(load! "+bindings")
