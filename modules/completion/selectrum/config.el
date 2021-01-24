;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! selectrum
  :hook (doom-first-input . selectrum-mode)
  :bind (:map selectrum-minibuffer-map
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate))
  :init
  (map! (:leader
         :desc "M-x" :ngv "<SPC>" #'execute-extended-command
         (:prefix "r"
          :desc "Resume Last" :ngv "l" #'selectrum-repeat)))

  (setq selectrum-extend-current-candidate-highlight t
        selectrum-fix-minibuffer-height t
        projectile-completion-system 'default
        selectrum-refine-candidates-function #'orderless-filter
        selectrum-highlight-candidates-function #'orderless-highlight-matches
        selectrum-preprocess-candidates-function #'selectrum-prescient--preprocess
        completion-styles '(substring partial-completion)))

;;;###package
(use-package! selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (selectrum-prescient-mode +1)
  (add-hook 'selectrum-candidate-selected-hook #'selectrum-prescient--remember)
  (add-hook 'selectrum-candidate-inserted-hook #'selectrum-prescient--remember))

;;;###package
(use-package! marginalia
  :after selectrum
  :hook (doom-first-input . marginalia-mode)
  :init (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

;;;###package
(use-package! orderless
  :config (setq completion-styles '(orderless)))

;;;###package
(use-package! consult-flycheck
  :when (featurep! :checkers syntax)
  :after (consult flycheck))

;;;###package
(use-package! consult
  :defer t
  :commands (consult-line consult-buffer)
  :init
  (map! (:leader
         (:prefix "a"
          :desc "Load Theme" :ng "T" #'consult-theme)
         (:prefix "b"
          :desc "Buffer List" :ng "b" #'consult-buffer
          :desc "Bookmarks" :ng "B" #'consult-bookmark)
         (:prefix "f"
          :desc "Find File" :ng "f" #'find-file
          :desc "Recent Files" :ng "r" #'consult-recent-file)
         (:prefix "h"
          :desc "Apropos" :ng "a" #'consult-apropos)
         (:prefix "i"
          :desc "Insert Char" :ng "c" #'insert-char)
         (:prefix "j"
          :desc "Outline" :ng "o" #'consult-outline
          :desc "iMenu" :ng "i" #'consult-imenu)
         (:prefix "p"
          :desc "Find File" :ng "f" #'projectile-find-file-dwim
          :desc "Switch Buffer" :ng "b" #'projectile-switch-to-buffer
          :desc "Switch Project" :ng "p" #'projectile-switch-project)
         (:prefix "s"
          :desc "Search Line" :ng "s" #'consult-line
          :desc "Search Line (default)" :ng "S" #'+consult/consult-line-default
          :desc "Locate" :ng "l" #'consult-locate
          :desc "Search Project" :ng "p" #'consult-ripgrep
          :desc "Search Project (default)" :ng "P" #'+consult/consult-ripgrep-project-default)))
  (fset 'multi-occur #'consult-multi-occur)
  (define-key!
    [remap apropos] #'consult-apropos
    [remap goto-line] #'consult-goto-line
    [remap imenu] #'consult-imenu
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    [remap man] #'consult-man
    [remap yank-pop] #'consult-yank-pop
    [remap locate] #'consult-locate
    [remap load-theme] #'consult-theme
    [remap recentf-open-files] #'consult-recent-file)
  :config
  (advice-add #'find-file :around #'doom-set-jump-a)
  (advice-add #'consult-goto-line :around #'doom-set-jump-a)
  (advice-add #'consult-buffer :around #'doom-set-jump-a)
  (advice-add #'consult-imenu :around #'doom-set-jump-a)
  (advice-add #'consult-ripgrep :around #'doom-set-jump-a)
  (setq consult-project-root-function #'doom-project-root))

;;;###package
(use-package! embark
  :defer t
  :commands (embark-act embark-act-noexcit embark-export)
  :init
  (define-key!
    "C-S-a" #'embark-act)
  (define-key! selectrum-minibuffer-map
    "C-c C-e" #'embark-export
    "C-c C-c" #'embark-act-noexit)
  :config
  (set-popup-rule! "^\\*Embark Collect\\*" :side 'bottom :size .5 :select t :modeline nil)
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;;;###package
(use-package! embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

;;;###package
(use-package! wgrep
  :after grep
  :commands (wgrep-change-to-wgrep-mode)
  :init (setq wgrep-auto-save-buffer t)
  :bind (:map grep-mode-map
         ("C-c C-e" . wgrep-change-to-wgrep-mode)
         ("q" . kill-buffer-and-window)
         :map wgrep-mode-map
         ("C-c C-c" . wgrep-finish-edit)
         ("<esc>" . wgrep-exit)))


(after! projectile
  ;; FIXME Why is this broken when ivy is disabled?
  (setq projectile-switch-project-action (lambda ()
                                           (+workspaces-set-project-action-fn)
                                           (+workspaces-switch-to-project-h))))
