;; -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

;;;###autoload
(defun +bl/open-efeed-files ()
  "Open all elfeed files."
  (interactive)
  (dolist (file rmh-elfeed-org-files)
    (let ((file (expand-file-name file org-directory)))
      (when (file-exists-p file)
        (find-file file)))))

;; http://doc.norang.ca/org-mode.html
;;;###autoload
(defun +bl/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun +bl/org-archive-if (condition-function)
  (if (funcall condition-function)
      (let ((next-point-marker
             (save-excursion (org-forward-heading-same-level 1) (point-marker))))
        (org-archive-subtree)
        (setq org-map-continue-from (marker-position next-point-marker)))))

;;;###autoload
(defun +bl/org-archive-if-completed ()
  (interactive)
  (+bl/org-archive-if 'org-entry-is-done-p))

;;;###autoload
(defun +bl/org-archive-completed-in-buffer ()
  "Archive all completed tasks in current buffer"
  (interactive)
  (org-map-entries '+bl/org-archive-if-completed))

;;;###autoload
(defun +bl/try-close-expired-task ()
  (require 'org-expiry)
  (unless (org-entry-is-done-p)
    (org-expiry-add-keyword)))

;;;###autoload
(defun +bl/close-expired-in-buffer ()
  "Close all tasks that have expired in the current buffer."
  (interactive)
  (org-map-entries '+bl/try-close-expired-task))

;;;###autoload
(defun +bl/expire-and-archive-tasks-in-buffer ()
  "Expire all expired tasks in buffer and archive all completed tasks in current buffer."
  (interactive)
  (+bl/close-expired-in-buffer)
  (save-buffer)
  (+bl/org-archive-completed-in-buffer))


(defun +bl/capture-properties-p ()
  "Check if we should auto capture properties or not"
  (not (derived-mode-p 'org-journal-mode)))

;;;###autoload
(defun +bl/insert-creation ()
  (when (+bl/capture-properties-p)
    (require 'org-expiry)
    (save-excursion
      (org-back-to-heading)
      (org-expiry-insert-created))))

;;;###autoload
(defun +bl/prepare-time (time)
  "Converts to active/inactive time stamp depending on the value of `'org-expiry-inactive-timestamps "
  (require 'org-expiry)
  (if org-expiry-inactive-timestamps
      (replace-regexp-in-string "<\\(.*?\\)>" "[\\1]" time)
    (replace-regexp-in-string "\\[\\(.*?\\)\\]" "<\\1>" time)))

;;;###autoload
(defun +bl/expire-on-scheduled ()
  "Sets the expiry date and time to be the same as the scheduled date+time of the entry."
  (interactive)
  (require 'org-expiry)
  (let* ((schedule (org-entry-get nil "SCHEDULED")))
    (when schedule
      (org-set-property org-expiry-expiry-property-name (+bl/prepare-time schedule)))))

;;;###autoload
(defun +bl/expire-on-deadline ()
  "Sets the expiry date and time to be the same as the deadline date+time of the entry."
  (interactive)
  (require 'org-expiry)
  (let* ((deadline (org-entry-get nil "DEADLINE")))
    (when deadline
      (org-set-property org-expiry-keyword (+bl/prepare-time deadline)))))

;;;###autoload
(defun +bl/expire-on-time-dwim ()
  "If there is a deadline for this entry, set the expiry to be the same as the deadline, otherwise set the expiry
to be that of the scheduled date+time."
  (interactive)
  (let* ((schedule (org-entry-get nil "SCHEDULED"))
         (deadline (org-entry-get nil "DEADLINE")))
    (if deadline
        (+bl/expire-on-deadline)
      (when schedule
        (+bl/expire-on-scheduled)))))

;;;###autoload
(defun +bl/capture-snippet ()
  "Formats a capture snippet for capturing code."
  (let ((line-number (line-number-at-pos (region-beginning)))
         (func-name (which-function))
         (org-src-mode (cdr (assoc major-mode +bl/major-mode-to-org-src))))
    (format "* %%?\nSource: [[file:%%F::%d][%%f (%s)]]\n#+begin_src %s\n%%i\n#+end_src" line-number func-name (or org-src-mode ""))))

;; TODO(Björn Larsson): Expand this list with more modes that should be supported
;;;###autoload
(setq +bl/major-mode-to-org-src
      '((c++-mode . "C++")
        (python-mode . "python")
        (emacs-lisp-mode . "emacs-lisp")
        (shell-mode . "sh")
        (lua-mode . "lua")
        (json-mode . "json")
        (yaml-mode . "yml")
        (cmake-mode . "cmake")))

;;;###autoload
(add-hook 'org-mode-hook #'flyspell-mode)
