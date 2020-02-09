;; -*- lexical-binding: t; -*-

(map! (:after projectile
        (:leader
          (:prefix ("p" . "project")
            :desc "Shell Command" :g "!" #'projectile-run-shell-command-in-root
            :desc "Async Shell Command" :g "&" #'projectile-run-async-shell-command-in-root
            :desc "Edit dir-locals" :g "e" #'projectile-edit-dir-locals
            :desc "Compile" :g "c" #'projectile-compile-project
            :desc "Configure" :g "C" #'projectile-configure-project
            :desc "Dired" :g "d" #'projectile-dired
            :desc "Kill Buffers" :g "k" #'projectile-kill-buffers
            :desc "Test Project" :g "t" #'projectile-test-project
            :desc "Shell" :g "s" #'projectile-run-vterm
            :desc "Find file in other project" :g "O" #'doom/find-file-in-other-project
            :desc "Project Scratch Buffer" :g "x" #'doom/open-project-scratch-buffer))))

(map! (:after persp-mode
        (:leader
          (:prefix ("p" . "project")
            :desc "Switch Perspective" :g "P" #'persp-switch))))
