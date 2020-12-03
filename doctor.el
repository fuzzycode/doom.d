;;; ~/Development/GitHub/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "git-imerge")
  (warn! "Unable to find git imerge executable, magit imerge will not work as expected."))

(unless (executable-find "pandoc")
  (warn! "pandoc executable not found. Make sure it is installed."))

(unless (executable-find "terminal-notifier")
  (warn! "terminal-notifier was not found. Make sure its installed to get nice notifications."))

(when (featurep! :completion ivy)
  (unless (executable-find "fzf")
    (warn! "fzf not found on your system, counsel-fzf will not work.")))
