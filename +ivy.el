;;; ~/.doom.d/+ivy.el -*- lexical-binding: t; -*-

(map! (:leader
        :desc "M-X" :g "<SPC>" #'counsel-M-x
        :desc "M-X" :g "M-<SPC>" #'counsel-M-x
        (:prefix ("b" . "buffer")
          :desc "Buffer List" :g "b" #'counsel-buffer-or-recentf)
        (:prefix ("f" . "files")
          :desc "Find File" :g "f" #'counsel-find-file
          :desc "Recent Files" :g "r" #'counsel-recentf)
        (:prefix ("j" . "jump/join")
          :desc "iMenu" :g "i" #'counsel-imenu)
        (:prefix ("p" . "project")
          :desc "Find Directory" :g "d" #'counsel-projectile-find-dir
          :desc "Find File" :g "f" #'counsel-projectile-find-file
          :desc "Switch Project" :g "p" #'counsel-projectile-switch-project
          :desc "Switch Buffer" :g "b" #'counsel-projectile-switch-to-buffer)
        (:prefix ("s" . "search")
          :desc "Swiper" :g "s" #'swiper)))
