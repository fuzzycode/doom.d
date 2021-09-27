;;; ~/Development/GitHub/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "git-imerge")
  (warn! "Unable to find git imerge executable, magit imerge will not work as expected."))

(unless (executable-find "pandoc")
  (warn! "pandoc executable not found. Make sure it is installed."))

(unless (executable-find "terminal-notifier")
  (warn! "terminal-notifier was not found. Make sure its installed to get nice notifications."))

(print! (start "Checking for installed fonts ... "))
(with-temp-buffer
  (cl-destructuring-bind (status . output)
      (doom-call-process "fc-list" "--format=%{family[0]}\n")
    (if (not (zerop status))
        (print! (error "Issue running `fc-list' on your system"))
      (insert output)
      (dolist (font (remove-duplicates (list doom-font doom-unicode-font doom-big-font doom-variable-pitch-font doom-serif-font)
                                       :test (lambda (lhs rhs) (eq (font-get lhs :family) (font-get rhs :family)))))
        (let ((family (format "%s" (font-get font :family))))
          (if (save-excursion (re-search-backward family nil t))
              (print! (success "Found font family: %s") family)
            (print! (warn "Unable to find font family: %s") family)))))))
