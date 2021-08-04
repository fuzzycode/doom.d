;; -*- lexical-binding: t; -*-

;; C++
(package! ninja-mode)
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))
(package! sourcetrail)

;; GIT
(package! magit-imerge)
(package! git-commit)
(package! gitignore-mode)
(package! gitconfig-mode)
(package! gitattributes-mode)
(package! rigid-tabs)
(package! gitignore-templates)

;;ORG
(package! demo-it)
(package! idle-org-agenda)
(package! org-super-agenda)
(package! doct)
(package! org-make-toc)
(package! org-ql)

;;ELISP
(package! eval-sexp-fu)

;; MAIL
;; (when (featurep! :email mu4e)
;;   (package! mu4e-maildirs-extension)
;;   (package! mu4e-alert))

;; (when (and (featurep! :email mu4e)
;;            (featurep 'xwidget-internal))
;;   (package! mu4e-views :recipe (:host github :repo "lordpretzel/mu4e-views")))

(package! visual-regexp-steroids)
(package! visual-regexp)
(package! sort-words)
;; (package! ace-isearch)
(package! smart-backspace)
(package! open-junk-file)
(package! winum)
;; (package! dash-at-point)
(package! pandoc-mode)
;; (package! avy)
(package! centered-cursor-mode)
;; (package! highlight-doxygen)
(package! smart-newline)
;; (package! rg)
(package! hardhat)

(when IS-MAC
  (package! reveal-in-osx-finder))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-surround))
