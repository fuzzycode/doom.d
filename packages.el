;; -*- lexical-binding: t; -*-


;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "2d9083b2608bd60c31583193d321d13a81a75beb")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "ad896213408f6961dd95ce384c36ebcced7b9f1a")

(package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15") ;; This is also a part of the csharp module but I only want this package from it
(package! metal-mode :recipe (:host github :repo "masfj/metal-mode") :pin "686ad916f53589d59797613c1050922dd424d492")
(package! cmake-font-lock :pin "a6038e916bcca807ae695f7d7e5c300c3f38f415")

;; GIT
(when (modulep! :tools magit)
  (when IS-WINDOWS
    (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))
  (package! diff-dired :recipe (:host github :repo "fuzzycode/diff-dired"))
  (package! magit-imerge :pin "5a1833d33e1516c7819521a5b35135527d9409f9")
  (package! rigid-tabs :pin "872a10c8751574c9610cba1800f541a6eda24997")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

(when (modulep! :tools magit +forge)
  (package! gh-notify))

;;ORG
(when (modulep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
  (package! vulpea :pin "f4d3448b6ccdb314c5fe3defea66e750e1371a10")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "1d98c7d07c6f2af5c84e8358cb6c5db71e8f1006")
  (package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
  (package! ox-asciidoc :pin "a55ac6adef39124c9434be47fe9cc0c75c4bfea2"))

;;ELISP
(when (modulep! :lang emacs-lisp)
  (package! inspector :pin "878e1696640793550998cbb941bba6430ac42629")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (modulep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "cdc2e141d8ecd59508a5cd50d6d02120073bf4f1"))

(when (and (modulep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "fa47f35e56edcc84f00d622e415ae970cc5df0dd"))

(when (modulep! :emacs dired)
  (package! dired+ :pin "90fc0d5a110063278e3fe320dd903ec07be5dba7"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! lang-mode :recipe (:host github :repo "fuzzycode/lang-mode"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! expand-region :pin "b70feaa644310dc2d599dc277cd20a1f2b6446ac")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "8f955abec9c1d75acd9b03389b90a276ec4e2137")
(package! centered-cursor-mode :pin "ebaeb80fba0bafdf6f95706308123dec2cf4b99f")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! string-inflection :pin "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c")
(package! ialign :pin "bc4d30d79f2f4b413288195ef19894ac0fd258b7")
(package! s :pin "e957dcb0677da18b2bb60ad867db5df5c35b5616")
(package! deadgrep :pin "67c0fd0afcc6c34a613bbf3298744228f1c3cd59")

(when (modulep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (modulep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
