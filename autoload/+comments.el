;;; doom.d/autoload/+comments.el -*- lexical-binding: t; -*-

(defun same-line-p (p1 p2)
  "Check if P1 and P2 are on the same line or not"
  (eq (count-lines 1 p1) (count-lines 1 p2)))

(defun one-line-region-p ()
  "Check if the region is confined to a single line or not"
  (and (region-active-p)
       (same-line-p (region-beginning) (region-end))))

;;;###autoload
(defun +core/comment-uncomment-dwim (arg)
  "A thin wrapper around `comment-dwim-2` that will create a block comment around
single line regions. Mostly used to comment/un-comment function parameters"
  (interactive "P")
  (require 'comment-dwim-2)
  (if (and (derived-mode-p 'c-mode 'c++-mode)
           (not arg)
           (one-line-region-p))
      (let ((comment-start "/*")
            (comment-end "*/")
            (comment-padding ""))
        (comment-dwim-2))
    (comment-dwim-2)))

;;;###autoload
(defun avy-comment-word (pt)
  "Comments a word at PT using avy."
  (save-excursion
    (set-mark (goto-char pt))
    (avy-forward-item)
    (call-interactively #'+core/comment-uncomment-dwim)))
