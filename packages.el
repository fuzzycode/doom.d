;; -*- lexical-binding: t; -*-

;;DISABLED

;; Stay on bleeding edge for lsp
(unpin! '(lsp-mode lsp-ui dap-modes))

;; C++
(package! ninja-mode)
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))

;; GIT
(package! magit-imerge)
(package! magit-tbdiff)
(package! git-commit)
(package! gitignore-mode)
(package! gitconfig-mode)
(package! gitattributes-mode)
(package! git-messenger)
(package! rigid-tabs)
(package! git-walktree)
(package! gitignore-templates)

;;ORG
(package! demo-it)
(package! idle-org-agenda)
(package! org-super-agenda)
(package! doct)
(package! org-make-toc)

;;ELISP
(package! elisp-format)
(package! eval-sexp-fu)

;; LATEX
(package! ebib)

;; CSV
(package! csv-mode)

(package! lsp-treemacs :pin nil)

;; MAIL
(when (featurep! :email mu4e)
  (package! mu4e-maildirs-extension)
  (package! mu4e-alert))

(when (and (featurep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :recipe (:host github :repo "lordpretzel/mu4e-views")))

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
(package! crux)
(package! dash-at-point)
(package! lorem-ipsum)
(package! string-inflection)
(package! alert)
(package! pandoc-mode)
(package! avy)
(package! goto-last-change)
(package! alert)
(package! centered-cursor-mode)
(package! highlight-doxygen)
(package! super-save)
(package! smart-newline)
(package! rg)
(package! hardhat)

(package! counsel-doxygen-snippets
  :recipe (:host github :repo "fuzzycode/counsel-doxygen-snippets"))

(when IS-MAC
  (package! reveal-in-osx-finder)
  (package! osx-dictionary))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-surround)
  (package! evil-collection))
