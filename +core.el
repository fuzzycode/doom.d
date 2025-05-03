;; -*- lexical-binding: t; -*-

;;
;; PACKAGES
;;
(after! flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                     ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                     ("Col" 3 nil :right-align t)
                                     ("Level" 8 flycheck-error-list-entry-level-<)
                                     ("ID" 35 t)
                                     (#("Message (Checker)" 9 16
                                        (face flycheck-error-list-checker-name))
                                      0 t)]))

(after! projectile
  (setq projectile-files-cache-expire 35)

  (put 'projectile-project-name 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'file-directory-p)

  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(after! warnings
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))


(after! smartparens
  (setq sp-escape-wrapped-region t
        sp-escape-quotes-after-insert t)

  (sp-local-pair 'c++-mode "/**" "*/" :actions '(navigate)) ;; Handle doxygen comment "pairs"

  (smartparens-strict-mode) ;; Start out in strict mode
  (show-smartparens-global-mode +1))

(after! doom-modeline
  (setq auto-revert-check-vc-info t
        doom-modeline-height 35 ;; default is 25 and that is a bit too small for me
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-indent-info t
        doom-modeline-check-simple-format nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-persp-name t
        doom-modeline-vcs-max-length 60))


(after! company
  (setq company-tooltip-minimum-width 80
        company-tooltip-maximum-width 80
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(after! vterm
  (setq vterm-shell (+bl/get-shell))

  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files))))

(after! recentf
  (add-to-list 'recentf-exclude "/private/var/folders/.*")
  (add-to-list 'recentf-exclude #'directory-name-p) ;; Filter out all directories from the list
  (add-to-list 'recentf-exclude doom-local-dir)  ;; Remove all files in the .local folders
  (add-to-list 'recentf-exclude "\\.gz$") ;; Remove zip archives
  (add-to-list 'recentf-exclude "\\.vrb$")) ;; Remove latex intermediate files


(after! consult
  (advice-add #'consult--jump :around #'doom-set-jump-a))

(after! evil-snipe
  (setq evil-snipe-spillover-scope 'whole-visible))

(after! evil-goggles
  (setq evil-goggles-duration 0.2))

(after! evil
  (setq evil-want-fine-undo t
        evil-kill-on-visual-paste nil
        evil-insert-state-message nil
        evil-visual-state-message nil))

(after! evil-surround
  (let ((pairs '((?g "$" . "$")
                 (?h "(" . ")")
                 (?j "{" . "}")
                 (?k "[" . "]")
                 (?l "<" . ">")
                 (?a "'" . "'")
                 (?s "\"" . "\""))))
    (setq-default evil-embrace-evil-surround-keys (append evil-embrace-evil-surround-keys (mapcar #'car pairs)))
    (setq-default evil-surround-pairs-alist (append evil-surround-pairs-alist pairs))))

(after! better-jumper
  (add-hook 'better-jumper-post-jump-hook #'recenter))

(after! lsp-ui
  (setq lsp-ui-peek-list-width 75)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-code-actions nil) ;; Prefer to have this in the mode-line
  (add-hook 'lsp-ui-mode-hook #'+bl/dim-lsp-sideline))

(after! lsp-mode
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-semantic-tokens-enable t)) ; Enable semantic highlighting by default

;; Completion
(after! corfu
  (setq corfu-preview-current 'insert
        corfu-auto t
        corfu-preselect 'valid))

;;
;; HOOKS
;;

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'help-mode-hook #'rainbow-mode)

(add-hook 'prog-mode-hook (lambda () (setq-local display-line-numbers-type 'relative)))


;;
;; SETTINGS
;;

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

(setq epg-pinentry-mode 'loopback)

(setq warning-minimum-level :error) ;; Only show popup on errors, warnings are still logged though

(setq plantuml-indent-level 2)

(setq ws-butler-convert-leading-tabs-or-spaces t)

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq ispell-dictionary "en_US")

;; Show the location of the sym-link and not the actual file
(setq find-file-visit-truename nil
      vc-follow-symlinks nil)

(setq x-stretch-cursor t
      ns-pop-up-frames nil
      create-lockfiles nil
      require-final-newline t
      use-dialog-box nil
      use-file-dialog nil)

;; iSeach
(setq isearch-lax-whitespace t
      search-whitespace-regexp ".*?"
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t)

;;iBuffer
(setq ibuffer-formats
      '((mark modified read-only " "
         (name 36 36 :left :elide) ; change: 30s were originally 18s
         " "
         (size 9 -1 :right)
         " "
         (mode 16 16 :left :elide)
         " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; Don't confirm closing emacs with running processes
(setq confirm-kill-processes nil)

;; Backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Compilation
(setq compilation-auto-jump-to-first-error nil
      compilation-skip-threshold 2)

;; Add extensionless file modes
(add-to-list 'auto-mode-alist '("\\.?zshenv\\(\\.local\\)?$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zshrc\\(\\.local\\)?$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zshenv$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?zprofile$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.?pep8$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig.local$" . gitconfig-mode))

;; Open documentation in webkit buffer
(when (featurep 'xwidget-internal)
  (after! dash-docs
    (setq dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))
  (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn))

(set-flyspell-predicate! '(c++-mode c-mode) #'+bl/cc-flyspell-predicate-p)

;; Enable advanced features without asking
(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode -1) ;; We never want that

(electric-indent-mode -1) ;; Not needed, newline and indent will take care of it

(pixel-scroll-mode t)

;; mac specifics
(when (featurep :system 'macos)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-right-command-modifier nil
        mac-right-option-modifier nil))

;; File templates
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)

;;
;; POPUPS
;;

(set-popup-rule! "^\\*Shell Command Output\\*$" :side 'bottom :height 40 :select nil :actions '(+bl/special-mode-action-fn))
(set-popup-rule! "^\\*YASnippet Tables\\*$" :quit 'other :side 'bottom :height 0.5 :select t)
(set-popup-rule! "^\\*Org Export Dispatcher\\*$" :side 'right :width 0.6)
(set-popup-rule! "^CAPTURE-.*\\.org$" :size 0.5 :quit nil :select t :autosave t)
(set-popup-rule! "^\\*xwidget" :vslot -11 :size 0.55 :select nil)
