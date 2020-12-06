;; -*- lexical-binding: t; -*-

(map! (:after projectile
        (:leader
          (:prefix ("p" . "project")
            :desc "Shell Command" :nvg "!" #'projectile-run-shell-command-in-root
            :desc "Async Shell Command" :nvg "&" #'projectile-run-async-shell-command-in-root
            :desc "Edit dir-locals" :nvg "e" #'projectile-edit-dir-locals
            :desc "Compile" :nvg "c" #'projectile-compile-project
            :desc "Configure" :nvg "C" #'projectile-configure-project
            :desc "Dired" :nvg "d" #'projectile-dired
            :desc "Kill Buffers" :nvg "k" #'projectile-kill-buffers
            :desc "Test Project" :nvg "t" #'projectile-test-project
            :desc "Shell" :nvg "s" #'projectile-run-vterm
            :desc "Find file in other project" :nvg "O" #'doom/find-file-in-other-project
            :desc "Project Scratch Buffer" :nvg "x" #'doom/open-project-scratch-buffer))))

(map! (:after persp-mode
        (:leader
          (:prefix ("p" . "project")
            :desc "Switch Perspective" :nvg "P" #'persp-switch))))

(after! projectile
  (global-set-key (kbd "M-o") #'projectile-find-file-dwim))
