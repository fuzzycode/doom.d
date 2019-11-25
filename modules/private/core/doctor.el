
(unless (executable-find "pandoc")
  (warn! "pandoc executable not found. Make sure it is installed."))

(unless (executable-find "terminal-notifier")
  (warn! "terminal-notifier was not found. Make sure its installed to get nice notifications."))
