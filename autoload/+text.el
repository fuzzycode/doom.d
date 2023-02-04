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
  ""
  (interactive (list (deadgrep--read-search-term)
                     (read-directory-name "Directory:" nil default-directory t nil)))
  (deadgrep search-term directory))
