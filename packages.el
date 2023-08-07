;; -*- lexical-binding: t; -*-


;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "36843d387cb0621c1a288179af223d4f1410be73")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))

(package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15") ;; This is also a part of the csharp module but I only want this package from it
(package! metal-mode :recipe (:host github :repo "masfj/metal-mode") :pin "686ad916f53589d59797613c1050922dd424d492")
(package! cmake-font-lock :pin "a6038e916bcca807ae695f7d7e5c300c3f38f415")

;; GIT
(when (modulep! :tools magit)
  (when IS-WINDOWS
    (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))
  (package! diff-dired :recipe (:host github :repo "fuzzycode/diff-dired"))
  (package! magit-imerge :pin "b7cfe49a197c2cf5948109921e053711b156389d")
  (package! magit-lfs :pin "cd9f46e1840270be27e2c2d9dcf036ff0781f66d")
  (package! rigid-tabs :pin "872a10c8751574c9610cba1800f541a6eda24997")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

(when (modulep! :tools magit +forge)
  (package! gh-notify :pin "e1afdd49deb8ddf1a988e8d3fa699b06dfa92f5f"))

;;ORG
(when (modulep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
  (package! idle-org-agenda :pin "8e6052fc4923c30132052d67d794b76c92851c20")
  (package! vulpea :pin "de199a16e294056e2368a2e031b19008cf9f9e52")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "eb5377320fcfd38354d6e9e3e655969ae3c0e052")
  (package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
  (package! ox-asciidoc :pin "3a8aad85c6df84155266ba5232f1cbadda8abc58"))

;;ELISP
(when (modulep! :lang emacs-lisp)
  (package! inspector :pin "5f32ed1f9f7e6b66b1c4027cdf71c113dd650226")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (modulep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "cdc2e141d8ecd59508a5cd50d6d02120073bf4f1"))

(when (and (modulep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "b02e360d36c54cd9a5f59cdf266be88214a966d2"))

(when (modulep! :emacs dired)
  (package! dired+ :pin "36d7958a7226fe26a62cf26cba5e37dd14ddab70"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! lang-mode :recipe (:host github :repo "fuzzycode/lang-mode"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "da3f0f5238a8b1e5f09f2ec97e683dc488d25be0")
(package! centered-cursor-mode :pin "ebaeb80fba0bafdf6f95706308123dec2cf4b99f")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! string-inflection :pin "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c")
(package! ialign :pin "bc4d30d79f2f4b413288195ef19894ac0fd258b7")
(package! deadgrep :pin "b36dc1b2e5d5e8cd53c1977eeea9b6469ad42381")

(package! chatgpt-shell) ;; Leave unpinned, it moves fast right now
(package! dall-e-shell)

(when (modulep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (modulep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
