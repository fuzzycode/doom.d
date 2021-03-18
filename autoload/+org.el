;; -*- lexical-binding: t; -*-


;;;###autoload
(defun +org/open-efeed-files ()
  "Open all elfeed files."
  (interactive)
  (dolist (file rmh-elfeed-org-files)
    (let ((file (expand-file-name file org-directory)))
      (when (file-exists-p file)
        (find-file file)))))

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
  "Archive all completed tasks in current buffer"
  (interactive)
  (org-map-entries '+org/org-archive-if-completed))

;;;###autoload
(defun +org/try-close-expired-task ()
  (require 'org-expiry)
  (unless (org-entry-is-done-p)
    (org-expiry-add-keyword)))

;;;###autoload
(defun +org/close-expired-in-buffer ()
  "Close all tasks that have expired in the current buffer."
  (interactive)
  (org-map-entries '+org/try-close-expired-task))

;;;###autoload
(defun +org/expire-and-archive-tasks-in-buffer ()
  "Expire all expired tasks in buffer and archive all completed tasks in current buffer."
  (interactive)
  (+org/close-expired-in-buffer)
  (save-buffer)
  (+org/org-archive-completed-in-buffer))


(defun +org/capture-properties-p ()
  "Check if we should auto capture properties or not"
  (not (derived-mode-p 'org-journal-mode)))

;;;###autoload
(defun +org/insert-creation ()
  (when (+org/capture-properties-p)
    (require 'org-expiry)
    (save-excursion
      (org-back-to-heading)
      (org-expiry-insert-created))))

;;;###autoload
(defun +org/insert-id ()
  (when (+org/capture-properties-p)
    (org-id-get-create)))

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
  (let* ((schedule (org-entry-get nil "SCHEDULED")))
    (when schedule
      (org-set-property org-expiry-expiry-property-name (+org/prepare-time schedule)))))

;;;###autoload
(defun +org/expire-on-deadline ()
  "Sets the expiry date and time to be the same as the deadline date+time of the entry."
  (interactive)
  (require 'org-expiry)
  (let* ((deadline (org-entry-get nil "DEADLINE")))
    (when deadline
      (org-set-property org-expiry-keyword (+org/prepare-time deadline)))))

;;;###autoload
(defun +org/expire-on-time-dwim ()
  "If there is a deadline for this entry, set the expiry to be the same as the deadline, otherwise set the expiry
to be that of the scheduled date+time."
  (interactive)
  (let* ((schedule (org-entry-get nil "SCHEDULED"))
         (deadline (org-entry-get nil "DEADLINE")))
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
        (org-insert-link nil (concat "id:" (car id)) (read-string "Description: " name))))))

;;;###autoload
(defun +org/org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat (pcase org-journal-file-type
            ('daily (format "#+TITLE: %s\n#+WEEK: %d\n#+AUTHOR: %s\n#+STARTUP: content\n\n* Expectations\n* Tasks\n* Entries\n* Summary\n"
                            (format-time-string org-journal-date-format time) (ts-week (ts-now)) user-full-name))
            ('weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
            ('monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
            ('yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

;;;###autoload
(defvar +org/org-journal--date-location-scheduled-time
"Store the last selected time for journaling, used to add todo time stamps." nil)

;;;###autoload
(defun +org/org-journal--scheduled-time-string ()
  "Return the scheduled time as a string."
  (format-time-string "%Y-%m-%d" +org/org-journal--date-location-scheduled-time))

;;;###autoload
(defun +org/org-journal-date-location (&optional scheduled-time)
  "Return the org journal file for DATE or ask user for a date. Creates the entry if missing."
  (require 'org-journal)
  (let* ((scheduled-time (or scheduled-time (org-read-date t t nil "Date:")))
         (file (org-journal--get-entry-path scheduled-time)))
    (setq +org/org-journal--date-location-scheduled-time scheduled-time)
    (unless (file-exists-p file)
      (org-journal-new-entry t scheduled-time)
      (save-buffer)
      (kill-buffer (current-buffer)))
    file))

;;;###autoload
(defun +org/org-journal-date-location-today ()
  "Get journal location for today."
  (+org/org-journal-date-location (org-read-date t t "")))

;;;###autoload
(defun +org/org-journal-date-location-tomorrow ()
  "Get journal location for tomorrow."
  (+org/org-journal-date-location (org-read-date t t "+1")))

;;;###autoload
(defun +org/org-journal-show-journal-today ()
  "Show today's journal, creating one if needed."
  (interactive)
  (org-journal-new-entry (org-read-date t t "")))

;;;###autoload
(defun +org/org-journal-show-journal-tomorrow ()
  "Show tomorrows journal."
  (interactive)
  (org-journal-new-entry (org-read-date t t "+1")))

;;;###autoload
(defun +org/org-journal-show-journal-yesterday ()
  "Show yesterdays journal"
  (interactive)
  (let ((path (org-journal--get-entry-path (org-read-date t t "-1"))))
    (if (file-exists-p path)
        (find-file path)
      (user-error "No journal found for yesterday"))))

;;;###autoload
(add-hook 'org-mode-hook #'flyspell-mode)
