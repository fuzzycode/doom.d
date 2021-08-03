;; -*- lexical-binding: t; -*-

;;DISABLED


;; C++
(package! ninja-mode)
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))
(package! sourcetrail)

;; GIT
(package! magit-imerge)
;; (package! magit-tbdiff)
;; (package! magit-delta)
(package! git-commit)
(package! gitignore-mode)
(package! gitconfig-mode)
(package! gitattributes-mode)
;; (package! git-messenger)
(package! rigid-tabs)
;; (package! git-walktree)
(package! gitignore-templates)

;;ORG
;; (package! demo-it)
;; (package! idle-org-agenda)
;; (package! org-super-agenda)
;; (package! doct)
;; (package! org-make-toc)
;; (package! org-ql)

;;ELISP
;; (package! elisp-format)
(package! eval-sexp-fu)

;; MAIL
;; (when (featurep! :email mu4e)
;;   (package! mu4e-maildirs-extension)
;;   (package! mu4e-alert))

;; (when (and (featurep! :email mu4e)
;;            (featurep 'xwidget-internal))
;;   (package! mu4e-views :recipe (:host github :repo "lordpretzel/mu4e-views")))

;; (package! beginend)
(package! visual-regexp-steroids)
(package! visual-regexp)
;; (package! sort-words)
;; (package! comment-dwim-2)
;; (package! ace-isearch)
(package! smart-backspace)
;; (package! mwim)
(package! open-junk-file)
;; (package! winum)
;; (package! dash-at-point)
;; (package! lorem-ipsum)
;; (package! string-inflection)
(package! pandoc-mode)
;; (package! avy)
;; (package! goto-last-change)
(package! centered-cursor-mode)
;; (package! highlight-doxygen)
;; (package! super-save)
;; (package! smart-newline)
;; (package! rg)
(package! hardhat)

(package! counsel-doxygen-snippets
  :recipe (:host github :repo "fuzzycode/counsel-doxygen-snippets"))

(when IS-MAC
  (package! reveal-in-osx-finder)
  (package! osx-dictionary))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-surround)
)
