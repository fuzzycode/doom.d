;; -*- lexical-binding: t; -*-


(package! magit-todos :disable t)
(package! ccls :disable t)
(package! rtags :disable t)

;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "903a05ce0e9befa8fb2767dd83a5a36499771087")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))
(package! tspew :recipe (:host github :repo "jefftrull/tspew" :files ("*.el")))

(package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15") ;; This is also a part of the csharp module but I only want this package from it
(package! metal-mode :recipe (:host github :repo "masfj/metal-mode") :pin "686ad916f53589d59797613c1050922dd424d492")
(package! cmake-font-lock :pin "a6038e916bcca807ae695f7d7e5c300c3f38f415")

;; GIT
(when (modulep! :tools magit)
  (when (featurep :system 'windows)
    (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))
  (package! diff-dired :recipe (:host github :repo "fuzzycode/diff-dired"))
  (package! magit-imerge :pin "34a057b452de7f856fd3bdef4a9e34309d2be9dc")
  (package! magit-lfs :pin "cd9f46e1840270be27e2c2d9dcf036ff0781f66d")
  (package! rigid-tabs :pin "9553118e76fcbc1d8f0bcb960de13c7e3f07b9df")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

;;ORG
(when (modulep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "51c9da5ce7b791150758984bab469d2222516844")
  (package! vulpea :pin "e1ea8480daf3e480effdd7ba3799126295a4a59a")
  (package! org-make-toc :pin "3ac2024694a9f974a7d263748642182fc7e829d1")
  (package! org-ql :pin "e41fe9018a4699532ec875bedddc9746f8e362aa")
  (package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")
  (package! ox-asciidoc :pin "d6736852a5479c73c253d2ea8b352dcb232d02f8"))

;;ELISP
(when (modulep! :lang emacs-lisp)
  (package! inspector :pin "7fd65dddac9875c2e30e364b8702c0d72915c3ac")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (modulep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "df5a7540d5eb55380417fb29694e97b064a4078f"))

(when (and (modulep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "b02e360d36c54cd9a5f59cdf266be88214a966d2"))

(when (modulep! :emacs dired)
  (package! dired+ :pin "d75d3d390b119b21016e5d0aaa2b78aeafba6cf3"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! lang-mode :recipe (:host github :repo "fuzzycode/lang-mode"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "c7fa568ab9cfbb2abfb9b22f419d28ce570d7b22")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! string-inflection :pin "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c")
(package! ialign :pin "bc4d30d79f2f4b413288195ef19894ac0fd258b7")
(package! deadgrep :pin "38abe362997d2f18633a75d04c09da751bf8085e")

(package! chatgpt-shell) ;; Leave unpinned, it moves fast right now
(package! dall-e-shell)

(when (modulep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when (featurep :system 'macos)
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (modulep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))


;; Allow local config packages
(load! "~/.packages.local.el" "" t)
