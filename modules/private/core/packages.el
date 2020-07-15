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
(package! highlight-doxygen)
(package! easy-kill)
(package! super-save)
(package! uuidgen)
(package! smart-newline)
(package! rg)

(package! counsel-doxygen-snippets
  :recipe (:host github :repo "fuzzycode/counsel-doxygen-snippets"))
(package! flycheck-clazy
  :recipe (:host github :repo "fuzzycode/flycheck-clazy"))

(when IS-MAC
  (package! reveal-in-osx-finder)
  (package! osx-dictionary))

(when (featurep! :completion helm)
  (package! helm-dash))
