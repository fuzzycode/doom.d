;;; ../Development/GitHub/dotfiles/doom.d/autoload/+text.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/string-inflection-cycle-dwim ()
  "String inflection cycle but smart about current mode"
  (interactive)
  (cond
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ((eq major-mode 'ruby-mode)
    (string-inflection-ruby-style-cycle))
   (t (string-inflection-all-cycle))))

;;;###autoload
(defun +bl/deadgrep-directory (search-term directory)
  "Search for SEARCH_TERM in DIRECTORY using deadgrep."
  (interactive (list (deadgrep--read-search-term)
                     (read-directory-name "Directory:" nil default-directory t nil)))
  (deadgrep search-term directory))

;; Back ported from emacs 30
;;;###autoload
(defun ispell-completion-at-point ()
  "Word completion function for use in `completion-at-point-functions'."
  (pcase (bounds-of-thing-at-point 'word)
    (`(,beg . ,end)
     (when (and (< beg (point)) (<= (point) end))
       (let* ((word (buffer-substring-no-properties beg end))
              (len (length word))
              (inhibit-message t)
              (all (cons word (ispell-lookup-words word)))
              (cur all))
         (while cur
           (unless (string-prefix-p word (car cur))
             (setcar cur (concat word (substring (car cur) len))))
           (while (when-let ((next (cadr cur)))
                    (not (string-prefix-p word next t)))
             (setcdr cur (cddr cur)))
           (setq cur (cdr cur)))
         (list beg end (cdr all)
               :annotation-function (lambda (_) " Dict. word")
               :exclusive 'no))))))
