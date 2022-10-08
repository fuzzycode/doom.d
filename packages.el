;; -*- lexical-binding: t; -*-


;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "3a6a4ed84bf4a310e5aa032d77844bfa6744c4cb")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "32bc21d4f0e011dbdb7dc1a9d1cd8651353f2943")

(package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15") ;; This is also a part of the csharp module but I only want this package from it

;; GIT
(when (modulep! :tools magit)
  (when IS-WINDOWS
    (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))
  (package! magit-imerge :pin "5a1833d33e1516c7819521a5b35135527d9409f9")
  (package! git-commit :pin "717171265026c0913a3611251f9d1be727a8890b")
  (package! rigid-tabs :pin "872a10c8751574c9610cba1800f541a6eda24997")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

;;ORG
(when (modulep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
  (package! doct :pin "506c22f365b75f5423810c4933856802554df464")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "5f70636556bffca92d8ef8297ba3002a4ab5b52d")
  (package! org-appear :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac")
  (package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
  (package! ox-asciidoc :pin "a55ac6adef39124c9434be47fe9cc0c75c4bfea2"))

;;ELISP
(when (modulep! :lang emacs-lisp)
  (package! emacs-inspector :recipe (:host github :repo "mmontone/emacs-inspector") :pin "0e89d28558f57db4519f154bb72ce617a8c6265d")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (modulep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "cdc2e141d8ecd59508a5cd50d6d02120073bf4f1"))

(when (and (modulep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "fa47f35e56edcc84f00d622e415ae970cc5df0dd"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! expand-region :pin "c5c4362741deebb0985a8a29f9b8b0e25160764a")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "0e1a50717599e813e2b872f2af9f40b2272793fd")
(package! centered-cursor-mode :pin "ebaeb80fba0bafdf6f95706308123dec2cf4b99f")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! fix-word :pin "02f9856e3fc8211cad8f6477431361f38ff04ab5")
(package! string-inflection :pin "50ad54970b3cc79b6b83979bde9889ad9a9e1a9c")
(package! ialign :pin "bc4d30d79f2f4b413288195ef19894ac0fd258b7")
(package! s :pin "e957dcb0677da18b2bb60ad867db5df5c35b5616")
(package! graphql-mode :pin "92136cf9b5a4dcd8c202c8dba9064b497776d2f7")
(package! vundo :pin "d78b02ab89955f3a3273884f6d799889a2ef6b6f")

(when (modulep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (modulep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
