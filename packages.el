;; -*- lexical-binding: t; -*-


(unpin! dired-git-info)
(unpin! code-review)

;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "c5e509481f1e53ceedc21d0315e125895b24d68d")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "32bc21d4f0e011dbdb7dc1a9d1cd8651353f2943")

(package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c") ;; This is also a part of the csharp module but I only want this package from it

;; GIT
(when (featurep! :tools magit)
  (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db")
  (package! magit-imerge :pin "1ee213d7fa1536c86c128d09946b44ededbfac9c")
  (package! git-commit :pin "613682a70631178caee15d1b5a16fae90da4af7b")
  (package! rigid-tabs :pin "eba84ceaba2e57e76ad2dfbb7a7154238a25d956")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

;;ORG
(when (featurep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "3108bc3f725818f0e868520d2c243abe9acbef4e")
  (package! doct :pin "4033a8fd8681d3989550f7a2532d6b4e3c45bfe8")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "af18eac2b80b2f56c135f37fcbdcce19fbc34b65")
  (package! org-appear :pin "ffbd742267ff81ba8433177fac5d7fe22b6d68a9")
  (package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
  (package! ox-asciidoc :pin "27bf9a3e900c782bd57719c81c0aa68d9a1e3b46"))

;;ELISP
(when (featurep! :lang emacs-lisp)
  (package! emacs-inspector :recipe (:host github :repo "mmontone/emacs-inspector"))
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (featurep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "1167bc6e08996f866e73e9a02f563fd21ac317fd"))

(when (and (featurep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "fa47f35e56edcc84f00d622e415ae970cc5df0dd"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! expand-region :pin "7e5bbe2763c12bae3e77fe0c49bcad05ff91dbfe")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "c1429887287b7ee9601196e26f97c908b6e4f5c0")
(package! centered-cursor-mode :pin "4093821cc9759ca5a3c6e527d4cc915fc3a5ad74")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! fix-word :pin "e967dd4ac98d777deeede8b497d6337634c06df4")
(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")
(package! ialign :pin "eca40b8b59ea713dba21b18f5b047a6c086b91dc")
(package! s :pin "08661efb075d1c6b4fa812184c1e5e90c08795a9")
(package! graphql-mode :pin "9740e4027bd9313697d5cac5caaa5b15626ab1da")

(when (featurep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
