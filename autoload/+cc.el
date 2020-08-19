;;; ../Development/GitHub/dotfiles/doom.d/autoload/+cc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-overlays-specifying (prop)
  "From https://www.gnu.org/software/emacs/manual/html_node/elisp/Finding-Overlays.html"
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))


;;;###autoload
(defun +cc-no-spell-check-includes-a (result)
  "Make sure that include statements are not spellchecked"
  (and result
       (not (find-overlays-specifying 'lsp-link))))
