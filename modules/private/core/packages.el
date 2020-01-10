;; -*- no-byte-compile: t; -*-

(package! paradox)
(package! beginend)
(package! visual-regexp-steroids)
(package! visual-regexp)
(package! sort-words)
(package! comment-dwim-2)
(package! ace-isearch)
(package! smart-backspace)
(package! mwim)
(package! open-junk-file)
(package! winum)
(package! deadgrep)
(package! fill-column-indicator)
(package! expand-region)
(package! insert-shebang)
(package! crux)
(package! dash-at-point)
(package! lorem-ipsum)
(package! string-inflection)
(package! alert)
(package! pandoc-mode)
(package! key-chord)
(package! avy)
(package! goto-last-change)
(package! use-package-chords)
(package! buffer-flip)
(package! alert)
(package! centered-cursor-mode)

(when IS-MAC
  (package! reveal-in-osx-finder)
  (package! osx-dictionary))

(when (featurep! :completion helm)
  (package! helm-dash))
