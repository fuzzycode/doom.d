;;; ../Workspace/dotfiles/doom.d/autoload/+cpp.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang cc)

(require 'dash)

(defun +bl/matches-in-buffer (regexp &optional buffer)
  "Return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches))
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (search-forward-regexp regexp nil t 1)
              (let ((linenr (line-number-at-pos)))
                (when (and matches (> linenr (+ 1 (car matches))))
                  (push ':endgroup matches))
                (push (line-number-at-pos) matches)))))
        (-split-on :endgroup matches))))

(defun +bl/sort-range (range)
  "Sort the lines given in the RANGE alphabetically"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((start (line-beginning-position (min (-first-item range) (-last-item range))))
            (end (line-end-position (max (-first-item range) (-last-item range)))))
        (sort-lines nil start end)))))

;;;###autoload
(defun +bl/sort-includes ()
  "Sort each group of includes alphabetically."
  (interactive)
  (dolist (group (+bl/matches-in-buffer "^\s*#include\s+[\"<].+?[\">].*$"))
    (+bl/sort-range group)))
