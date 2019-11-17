
(map! (:after projectile
	(:leader
        (:prefix ("p" . "project")
          :desc "Shell Command" :g "!" 'projectile-run-shell-command-in-root
          :desc "Async Shell Command" :g "&" 'projectile-run-async-shell-command-in-root
          :desc "Edit dir-locals" :g "e" 'projectile-edit-dir-locals
          :desc "Compile" :g "c" 'projectile-compile-project
          :desc "Configure" :g "C" 'projectile-configure-project
          :desc "Dired" :g "d" 'projectile-dired
          :desc "Kill Buffers" :g "k" 'projectile-kill-buffers
          :desc "Test Project" :g "t" 'projectile-test-project))))
