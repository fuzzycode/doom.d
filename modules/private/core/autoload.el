;;; private/core/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +core/inflection-cycle-dwim ()
  "switching by major-mode"
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    (string-inflection-ruby-style-cycle))))

;;;###autoload
(defun +core/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode spacemacs-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;;;###autoload
(defun +core/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;###autoload
(defun +core/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; http://stackoverflow.com/a/10216338/4869
;;;###autoload
(defun +core/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;;;###autoload
(defun +core/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;###autoload
(defun +core/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;;###autoload
(defun +core/switch-to-message-buffer ()
  "Switch to the `*Messages*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

;;;###autoload
(defun +core/uniquify-lines-buffer ()
  "Uniwuify lines in whole buffer"
  (interactive)
  (save-excursion
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (delete-duplicate-lines)))

;;;###autoload
(defun +core/uniquify-lines-dwim ()
  "Uniquify lines in region if active and in buffer if not."
  (interactive)
  (if (region-active-p)
      (delete-duplicate-lines)
    (+core/uniquify-lines-buffer)))

;;;###autoload
(defun +core/open-junk-file ()
      (interactive)
      (let* ((fname (format-time-string open-junk-file-format (current-time)))
             (junk-dir (file-name-directory fname))
             (default-directory junk-dir))

        (require 'counsel)
        (counsel-find-file rel-fname)))

;;;###autoload
(defun +core/browse-junk-files ()
  (interactive)
  (dired (concat doom-private-dir "junk/")))

;;;###autoload
(defun +core/disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

;;;###autoload
(add-hook 'minibuffer-setup-hook #'+core/disable-key-chord-mode)

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
;;;###autoload
(defun +core/bury-compile-buffer-if-successful (buffer string)
  (if (and
       (null (string-match ".*exited abnormally.*" string))
       (bound-and-true-p  bl-edit-close-compile-on-success))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        (run-at-time
         (format "%d sec" bl-edit-compile-auto-close-time) nil 'delete-windows-on
         (get-buffer-create "*compilation*")))))

;;;###autoload
(add-hook 'compilation-finish-functions #'+core/bury-compile-buffer-if-successful)

;;;###autoload
(defun +core/maybe-notify-compile-finish (buffer string)
  "Show an alert when compilation finished, like XCode does"
  (require 'alert)
  (if (string-match "^finished" string)
      (alert "Compilation finished OK!" :title "Compilation Successful" :category 'compile :id 'compile-ok)
    (alert "Compilation Failed" :title "Compilation Failed" :category 'compile :id 'compile-fail)))

;;;###autoload
(add-hook 'compilation-finish-functions #'+core/maybe-notify-compile-finish)

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
single line regions. Mostly used to comment/UN-comment function paramaters"
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
