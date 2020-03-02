
(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-indent-info t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-checker-simple-format nil
        doom-modeline-persp-name t
        doom-modeline-persp-name-icon t
        doom-modeline-lsp t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-version t
        doom-modeline-mu4e t
        doom-modeline-vcs-max-length 45)

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker " " bar)))
