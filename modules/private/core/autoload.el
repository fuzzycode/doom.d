;;; private/core/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +core/compile-auto-close-time 2
  "Time in seconds to leave a successful compile buffer open.")

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
(defun +core/disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

;;;###autoload
(add-hook 'minibuffer-setup-hook #'+core/disable-key-chord-mode)

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
;;;###autoload
(defun +core/bury-compile-buffer-if-successful (_buffer string)
  (if (null (string-match ".*exited abnormally.*" string))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        (run-at-time
         (format "%d sec" +core/compile-auto-close-time) nil 'delete-windows-on
         (get-buffer-create "*compilation*")))))

;;;###autoload
(add-hook 'compilation-finish-functions #'+core/bury-compile-buffer-if-successful)

;;;###autoload
(defun +core/maybe-notify-compile-finish (_buffer string)
  "Show an alert when compilation finished, like XCode does"
  (require 'alert)
  (when (not (memq major-mode '(rg-mode))) ;; No need to alert after each search
    (if (string-match "^finished" string)
        (alert "Compilation finished OK!" :title "Compilation Successful" :category 'compile :id 'compile-ok)
      (alert "Compilation Failed" :title "Compilation Failed" :category 'compile :id 'compile-fail))))

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

;;;###autoload
(defun +core/disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;###autoload
(defun +core/disable-themes (&rest _args)
  (+core/disable-all-themes))

;;;###autoload
(advice-add #'load-theme :before #'+core/disable-themes)

;;;###autoload
(add-hook #'doom-load-theme-hook (lambda () (set-face-underline 'show-paren-match t))) ;; Always underline matching parens
