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
(add-hook 'org-mode-hook #'flyspell-mode)

;; Capture Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun +org/make-org-todo-template (&key (content "%?") (creation-state "TODO") (project nil))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert content)
    (org-todo creation-state)
    (org-set-property "CREATED"
                      (with-temp-buffer
                        (org-insert-time-stamp
                         (org-current-effective-time) t t)))
    (when project
      (org-set-property "PROJECT" project))
    (remove-hook 'post-command-hook 'org-add-log-note)
    (let ((org-log-note-purpose 'state)
          (org-log-note-return-to (point-marker))
          (org-log-note-marker (progn (goto-char (org-log-beginning t))
                                      (point-marker)))
          (org-log-note-state creation-state))
      (org-add-log-note))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun +org/make-org-todo-idea ()
  (+org/make-org-todo-template :creation-state "IDEA"))

;;;###autoload
(defun +org/make-org-todo-in-progress ()
  (+org/make-org-todo-template :creation-state "IN-PROGRESS"))

;;;###autoload
(cl-defun +org/make-org-note-template (&key (content "%?"))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert content)
    (org-set-property "CREATED"
                      (with-temp-buffer
                        (org-insert-time-stamp
                         (org-current-effective-time) t t)))
    (buffer-substring-no-properties (point-min) (point-max))))
