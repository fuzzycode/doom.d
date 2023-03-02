;; -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

(defvar +bl/org-roam-file-fomat "%<%Y%m%d%H%M%S>-${slug}.org")
(defvar +bl/org-roam-project-template "#+title: ${title}\n#+date:%U\n#+category: ${title}\n#+filetags: :project:\n\n")
(defvar +bl/org-roam-default-template  "#+title: ${title}\n#+date: %U\n")

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

;; coppied and adapted from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
;;;###autoload
(defun +bl/org-roam-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

;;;###autoload
(defun +bl/org-roam-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (require 'vulpea)
  (when (and (not (active-minibuffer-window))
             (+bl/org-roam-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (+bl/org-roam-project-p)
            (setq tags (cons "work" tags))
          (setq tags (remove "work" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

;;;###autoload
(add-hook 'find-file-hook #'+bl/org-roam-project-update-tag)

;;;###autoload
(add-hook 'before-save-hook #'+bl/org-roam-project-update-tag)

(defun +bl/org-roam-maybe-get-project-name (&optional dir)
  ""
  (if (projectile-project-p dir)
      (projectile-project-name dir)
    ""))

;;;###autoload
(defun +bl/org-roam-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

;;;###autoload
(defun +bl/org-roam-project-files ()
  "Return a list of note files containing `work' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"work\"%"))]))))

;;;###autoload
(defun +bl/org-roam-agenda-files-update-a (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (+bl/org-roam-project-files)))

;;;###autoload
(defun +bl/org-roam-update-agenda-files ()
  "Interactive function if I ever need to manually update agenda files."
  (interactive)
  (+bl/org-roam-agenda-files-update-a))

;;;###autoload
(advice-add 'org-agenda :before #'+bl/org-roam-agenda-files-update-a)

;;;###autoload
(advice-add 'org-todo-list :before #'+bl/org-roam-agenda-files-update-a)

(defun +bl/org-roam-filter-by-tag (tag)
  "Generator function to generate filter function using TAG."
  (lambda (node)
    (member tag (org-roam-node-tags node))))

;;;###autoload
(defun +bl/org-roam-find-project ()
  "Add or open a note about a project."
  (interactive)
  (let ((project-name (+bl/org-roam-maybe-get-project-name)))
    (org-roam-node-find nil
                        project-name
                        (+bl/org-roam-filter-by-tag "Project")
                        nil
                        :templates
                        `(("p" "project" plain "\n* Tasks\n\n* Links\n\n"
                           :target (file+head ,+bl/org-roam-file-fomat ,+bl/org-roam-project-template)
                           :unnarrowed t)))))

;;;###autoload
(defun +bl/org-roam-capture-inbox ()
  "Capture something to the inbox"
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :target (file+head "inbox.org" "#+category: Inbox\n#+title: Inbox\n#+filetags: :INBOX:\n")))))

;;;###autoload
(defun +bl/org-roam-open-inbox ()
  ""
  (interactive)
  (let ((file-path (expand-file-name "Inbox.org" org-roam-directory)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "No Inbox file found."))))

;;;###autoload
(defun +bl/org-roam-capture-project-task ()
  ""
  (interactive)
  (let ((project-name (+bl/org-roam-maybe-get-project-name)))
    (org-roam-capture- :node (org-roam-node-read
                              project-name
                              (+bl/org-roam-filter-by-tag "Project"))
                       :templates `(("p" "project" plain "** TODO %?"
                                     :target (file+head+olp ,+bl/org-roam-file-fomat ,+bl/org-roam-project-template ("Tasks")))))))
;;;###autoload
(defun +bl/org-roam-capture-default ()
  ""
  (interactive)
  (let ((templates `(("d" "default" plain "%?"
                      :target (file+head ,+bl/org-roam-file-fomat ,+bl/org-roam-default-template)))))
    (org-roam-capture :keys "d" :templates templates)))

;;;###autoload
(defun +bl/org-roam-node-insert-immediate (arg &rest args)
  ""
  (interactive "P")
  (let ((args (push arg args))
        org-roam-capture-templates `(("d" "default" plain "%?"
                                      :immediate-finish t
                                      :target (file+head ,+bl/org-roam-file-fomat ,+bl/org-roam-default-template))))
    (apply #'org-roam-node-insert args)))

;;; https://github.com/tecosaur/emacs-config/blob/master/config.org#modeline-file-name
;;;###autoload
(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

;;;###autoload
(defun +bl/org-roam-capture-snippet ()
  "Capture the selected region as a code snippet."
  (interactive)
  (if (region-active-p)
      (let* ((line-number (line-number-at-pos (region-beginning)))
            (func-name (which-function))
            (babel-name (replace-regexp-in-string "\\(.*\\)-mode$" "\\1" (format "%s" major-mode)))
            (templates `(("c" "code" plain ,(format "\n* Description\n%%?\n* Source\n[[file:%%F::%d][%%f%s]]\n#+begin_src %s\n%%i#+end_src"
                                                   line-number
                                                   (if func-name (format " (%s)" func-name) "")
                                                   (or babel-name ""))
                          :unnarrowed t
                          :target (file+head ,+bl/org-roam-file-fomat "#+title: ${title}\n#+date: %U\n#+filetags: :code:")))))

        (org-roam-capture :keys "c" :templates templates))
    (message "No region highlighted")))

;;;###autoload
(add-hook 'dired-mode-hook #'org-download-enable)

;;;###autoload
(add-hook 'org-mode-hook #'flyspell-mode)
