
(use-package! helm-mode
  :defer 1
  ;:after-call pre-command-hook
  :init
  (map! [remap apropos]                   #'helm-apropos
        [remap find-library]              #'helm-locate-library
        [remap bookmark-jump]             #'helm-bookmarks
        [remap execute-extended-command]  #'helm-M-x
        [remap find-file]                 #'helm-find-files
        [remap locate]                    #'helm-locate
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap persp-switch-to-buffer]    #'+helm/workspace-mini
        [remap switch-to-buffer]          #'helm-buffers-list
        [remap projectile-find-file]      #'+helm/projectile-find-file
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap recentf-open-files]        #'helm-recentf
        [remap yank-pop]                  #'helm-show-kill-ring)
  :config
  (helm-mode +1)
  ;; helm is too heavy for `find-file-at-point'
  ;(add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil))
  )


(use-package! helm
  :after helm-mode
  :bind (("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         :map helm-find-files-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-<tab>" . helm-find-files-up-one-level)
         :map helm-map
         ("C-z" . helm-select-action))

  :init
  (setq helm-prevent-escaping-from-minibuffer t
        helm-move-to-line-cycle-in-source     t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-inside-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-ff-lynx-style-map nil)


  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-M-x-fuzzy-match fuzzy
          helm-ag-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-mode-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))


    (map! (:leader
          :desc "M-X" :g "<SPC>" 'helm-M-x
          (:prefix ("h" . "Help")
            (:prefix ("d" . "Describe")
              :desc "Apropos" :g "a" 'helm-apropos
              :desc "Function" :g "f" 'describe-function
              :desc "Key" :g "k" 'describe-key
              :desc "Mode" :g "m" 'describe-mode
              :desc "Package" :g "p" 'describe-package
              :desc "Theme" :g "t" 'describe-theme
              :desc "Variable" :g "v" 'describe-variable))
          (:prefix ("f" . "files")
            )
          ))

    (add-hook 'helm-minibuffer-set-up-hook
          '+helm//helm-hide-minibuffer-maybe)
    :config

  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.35 :ttl nil)

  ;; HACK Doom doesn't support these commands, which invite the user to install
  ;; the package via ELPA. Force them to use +helm/* instead, because they work
  ;; out of the box.
  (advice-add #'helm-projectile-rg :override #'+helm/rg)
  (advice-add #'helm-projectile-ag :override #'+helm/ag)
  (advice-add #'helm-projectile-grep :override #'+helm/grep)

  ;; Hide the modeline
  (defun +helm--hide-mode-line (&rest _)
    (with-current-buffer (helm-buffer-get)
      (unless helm-mode-line-string
        (hide-mode-line-mode +1))))
  (add-hook 'helm-after-initialize-hook #'+helm--hide-mode-line)
  (advice-add #'helm-display-mode-line :override #'+helm--hide-mode-line)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore)

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a)))


(use-package! helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))


;;;###package helm-projectile
(use-package! helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-find-dir
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  ;(defvar helm-projectile-find-file-map (make-sparse-keymap))
  (map! (:leader (:prefix ("p" . "project")
                   :desc "Find Directory" :g "d" 'helm-projectile-find-dir
                   :desc "Find file" :g "f" 'helm-projectile-find-file
                   :desc "Recent Files" :g "F" 'helm-projectile-recentf
                   :desc "Switch Project" :g "p" 'helm-projectile-switch-project
                   :desc "Buffer" :g "b" 'helm-projectile-switch-to-buffer)))
  )
                                        ;:config
  ;(set-keymap-parent helm-projectile-find-file-map helm-map))

(use-package! helm-swoop
  :defer t
  :init (map! (:leader
                (:prefix ("s" . "search")
                  :desc "Swoop" :g "s" 'helm-swoop-without-pre-input
                  :desc "Swoop w. Input" :g "S" 'helm-swoop))))
