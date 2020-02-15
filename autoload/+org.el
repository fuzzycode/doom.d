;; http://doc.norang.ca/org-mode.html
;;;###autoload
(defun +org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun +org/org-archive-if (condition-function)
  (if (funcall condition-function)
      (let ((next-point-marker
             (save-excursion (org-forward-heading-same-level 1) (point-marker))))
        (org-archive-subtree)
        (setq org-map-continue-from (marker-position next-point-marker)))))

;;;###autoload
(defun +org/org-archive-if-completed ()
  (interactive)
  (+org/org-archive-if 'org-entry-is-done-p))

;;;###autoload
(defun +org/org-archive-completed-in-buffer ()
  (interactive)
  (org-map-entries '+org/org-archive-if-completed))

;;;###autoload
(defun +org/insert-creation ()
  (save-excursion
    (org-back-to-heading)
    (org-expiry-insert-created)))


;;;###autoload
(add-hook 'org-mode-hook #'flyspell-mode)
