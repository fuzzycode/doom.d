;;; ~/.doom.d/+ivy.el -*- lexical-binding: t; -*-

(map! (:leader
        :desc "M-X" :g "<SPC>" #'counsel-M-x
        :desc "M-X" :g "M-<SPC>" #'counsel-M-x
        (:prefix ("b" . "buffer/bookmarks")
          :desc "Buffer List" :g "b" #'counsel-buffer-or-recentf
          :desc "Bookmarks" :g "B" #'counsel-bookmark)
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
        (:prefix ("r" . "resume")
          :desc "Ivy Resume Last" :g "l" #'ivy-resume)
        (:prefix ("s" . "search")
          :desc "Swiper" :g "s" #'swiper)))

(after! ivy
  (setq ivy-count-format "(%d/%d) "
        ivy-wrap nil
        ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-display-style 'plain))

