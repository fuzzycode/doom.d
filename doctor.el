;;; ~/Development/GitHub/dotfiles/doom.d/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "git-imerge")
  (warn! "Unable to find git imerge executable, magit imerge will not work as expected."))
