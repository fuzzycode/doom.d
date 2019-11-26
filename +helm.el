
(map! (:leader
        (:prefix "s"
          :desc "Search Project" :g "p" #'+helm/project-search
          :desc "Search Project (input)" :g "P" #'+helm/helm-search-project-default)
        (:prefix ("p" . "project")
                 :desc "Find Directory" :g "d" #'helm-projectile-find-dir
                 :desc "Find file" :g "f" #'helm-projectile-find-file
                 :desc "Recent Files" :g "F" #'helm-projectile-recentf
                 :desc "Switch Project" :g "p" #'helm-projectile-switch-project
                 :desc "Switch Buffer" :g "b" #'helm-projectile-switch-to-buffer)))

(map! (:leader
        :desc "M-X" :g "<SPC>" #'helm-M-x
        :desc "M-X" :g "M-<SPC>" #'helm-M-x
        (:prefix ("h" . "help")
          :desc "Apropos" :g "a" #'helm-apropos
          :desc "Info at Point" :g "i" #'helm-info-at-point
          :desc "Man/Woman" :g "m" #'helm-man-woman
          (:prefix ("d" . "describe")
            :desc "Function" :g "f" #'describe-function
            :desc "Key" :g "k" #'describe-key
            :desc "Mode" :g "m" #'describe-mode
            :desc "Package" :g "p" #'describe-package
            :desc "Theme" :g "t" #'describe-theme
            :desc "Variable" :g "v" #'describe-variable))
        (:prefix ("b" "buffer")
          :desc "Buffer List" :g "b" #'helm-buffer-list)
        (:prefix ("f" . "files")
          :desc "Find File" :g "f" #'helm-find-files-1
          :desc "Find Files" :g "F" #'helm-find-files
          :desc "Locate" :g "l" #'helm-locate
          :desc "Recent Files" :g "r" #'helm-recentf)
        (:prefix ("r" . "resume/registers")
          :desc "Resume Search" :g "l" #'helm-resume
          :desc "Registers" :g "r" #'helm-register)
        (:prefix ("i" . "insert")
          :desc "Unicode Char" :g "U" #'helm-ucs)
        (:prefix ("j" "jump/join")
          :desc "Imenu" :g "i" #'helm-imenu
          :desc "Imenu all Buffers" :g "I" #'helm-imenu-in-all-buffers)))

(map! (:after helm
        :map helm-find-files-map
        "<tab>"  #'helm-execute-persistent-action
        "C-<tab>"  #'helm-find-files-up-one-level
        :map helm-map
        "C-z"  #'helm-select-action))


(set-popup-rule! "^\\*helm" :vslot -100 :size 0.35 :ttl nil)


(after! helm
  (setq helm-buffer-max-length 45
        helm-candidate-number-limit 100
        helm-prevent-escaping-from-minibuffer t
        helm-move-to-line-cycle-in-source     t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-inside-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-move-to-line-cycle-in-source nil
        helm-ff-lynx-style-map nil))

;; Packages

;;;###package
(use-package! helm-swoop
  :defer t
  :init (map! (:leader
                (:prefix ("s" . "search")
                  :desc "Swoop" :g "s" 'helm-swoop-without-pre-input
                  :desc "Swoop w. Input" :g "S" 'helm-swoop))))

;;;###package
(use-package! helm-descbinds
  :defer t
  :init (map! (:leader
                (:prefix ("h" . "help")
                  (:prefix ("d" . "describe")
                    :desc "Bindings" :g "?" 'helm-descbinds)))))

;;;###package
(use-package helm-gitignore
    :defer t
    :init
    (map! (:leader
            (:prefix ("g" . "Git")
              :desc "Git Ignore" :g "I" 'helm-gitignore))))
