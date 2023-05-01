;;; ../Workspace/dotfiles/doom.d/autoload/+shell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/get-shell ()
  "Find the \"best\" shell to use for example vterm."
  (let ((shells '("fish" "zsh" "bash"))
        (default (or (getenv "SHELL") "/bin/zsh")))
    (seq-find #'identity (map 'list #'executable-find shells) default)))
