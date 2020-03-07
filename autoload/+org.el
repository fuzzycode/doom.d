;; -*- lexical-binding: t; -*-

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
(defun +org/try-close-expired-task ()
  (require 'org-expiry)
  (unless (org-entry-is-done-p)
    (org-expiry-add-keyword)))

;;;###autoload
(defun +org/close-expired-in-buffer ()
  ""
  (interactive)
  (org-map-entries '+org/try-close-expired-task))

;;;###autoload
(defun +org/expire-and-archive-tasks-in-buffer ()
  ""
  (interactive)
  (+org/close-expired-in-buffer)
  (save-buffer)
  (+org/org-archive-completed-in-buffer))

;;;###autoload
(defun +org/insert-creation ()
  (require 'org-expiry)
  (save-excursion
    (org-back-to-heading)
    (org-expiry-insert-created)))

;;;###autoload
(defun +org/prepare-time (time)
  "Converts to active/inactive time stamp depending on the value of `'org-expiry-inactive-timestamps "
  (require 'org-expiry)
  (if org-expiry-inactive-timestamps
      (replace-regexp-in-string "<\\(.*?\\)>" "[\\1]" time)
    (replace-regexp-in-string "\\[\\(.*?\\)\\]" "<\\1>" time)))

;;;###autoload
(defun +org/expire-on-scheduled ()
  "Sets the expiry date and time to be the same as the scheduled date+time of the entry."
  (interactive)
  (require 'org-expiry)
  (let* ((properties (org-entry-properties))
         (schedule (assoc "SCHEDULED" properties)))
    (when schedule
      (org-set-property org-expiry-keyword (+org/prepare-time (cdr schedule))))))

;;;###autoload
(defun +org/expire-on-deadline ()
  "Sets the expiry date and time to be the same as the deadline date+time of the entry."
  (interactive)
  (require 'org-expiry)
  (let* ((properties (org-entry-properties))
         (deadline (assoc "DEADLINE" properties)))
    (when deadline
      (org-set-property org-expiry-keyword (+org/prepare-time (cdr deadline))))))

;;;###autoload
(defun +org/expire-on-time-dwim ()
  "If there is a deadline for this entry, set the expiry to be the same as the deadline, otherwise set the expiry
to be that of the scheduled date+time."
  (interactive)
  (let* ((properties (org-entry-properties))
         (schedule (assoc "SCHEDULED" properties))
         (deadline (assoc "DEADLINE" properties)))
    (if deadline
        (+org/expire-on-deadline)
      (when schedule
        (+org/expire-on-scheduled)))))

;;;###autoload
(defun +core/capture-snippet ()
  "Formats a capture snippet for capturing code."
  (let ((line-number (line-number-at-pos (region-beginning)))
         (func-name (which-function))
         (org-src-mode (cdr (assoc major-mode +org/major-mode-to-org-src))))
    (format "* %%?\nSource: [[file:%%F::%d][%%f (%s)]]\n#+begin_src %s\n%%i\n#+end_src" line-number func-name (or org-src-mode ""))))

;; TODO(Björn Larsson): Expand this list with more modes that should be supported
;;;###autoload
(setq +org/major-mode-to-org-src
      '((c++-mode . "C++")
        (python-mode . "python")
        (emacs-lisp-mode . "emacs-lisp")
        (shell-mode . "sh")
        (lua-mode . "lua")
        (json-mode . "json")
        (yaml-mode . "yml")
        (cmake-mode . "cmake")))

;;;###autoload
(defun +org/project-org-file-name (&optional project-name)
  (let ((file (or project-name (projectile-project-name))))
    (concat (string-remove-prefix "." file) ".org")))

;;;###autoload
(defun +org/projects-directory ()
  (concat org-directory "/projects/"))

;;;###autoload
(defun +org/project-org-file-path (&optional project-name)
  (concat (+org/projects-directory) (+org/project-org-file-name project-name)))


;;;###autoload
(defun +org/extract-id-info ()
  "Extracts item and id if the item has an id."
  (let ((id (org-entry-get nil "ID"))
        (title (org-entry-get nil "ITEM")))
    (when id
      `(,title ,id))))

;;;###autoload
(defun +org/org-global-custom-ids ()
  "Find custom ID fields in all org agenda files."
  (require 'dash)
  (-non-nil (org-map-entries #'+org/extract-id-info t 'agenda-with-archives)))

;;;###autoload
(defun +org/org-insert-custom-id-link ()
  "Insert an Org link to a custom ID selected interactively."
  (interactive)
  (let* ((all-ids (+org/org-global-custom-ids))
        (custom-id (completing-read "Select Item: " all-ids)))
    (when custom-id
      (let* ((id-parts (assoc custom-id all-ids))
             (name (car id-parts))
             (id (cdr id-parts)))
        (org-insert-link nil (concat "id:" (car id)) name)))))

;;;###autoload
(add-hook 'org-mode-hook #'flyspell-mode)
