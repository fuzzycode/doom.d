;; -*- lexical-binding: t; -*-


(unpin! dired-git-info)
(unpin! code-review)

;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "55f54511d35716c43637dee2bcb5fbc7839f967b")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "32bc21d4f0e011dbdb7dc1a9d1cd8651353f2943")

(package! shader-mode :pin "d7dc8d0d6fe8914e8b6d5cf2081ad61e6952359c") ;; This is also a part of the csharp module but I only want this package from it

;; GIT
(when (featurep! :tools magit)
  (when IS-WINDOWS
    (package! ssh-agency :pin "a5377e4317365a3d5442e06d5c255d4a7c7618db"))
  (package! magit-imerge :pin "37bca48218dc32cad964e01e0f9936a90f634fba")
  (package! git-commit :pin "a7953b2645503904b2a31e18e019f07af9e71a7a")
  (package! rigid-tabs :pin "872a10c8751574c9610cba1800f541a6eda24997")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

;;ORG
(when (featurep! :lang org)
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "3108bc3f725818f0e868520d2c243abe9acbef4e")
  (package! doct :pin "8464809754f3316d5a2fdcf3c01ce1e8736b323b")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "67506a56f8e84af80af922e40797f8dc861243e4")
  (package! org-appear :pin "8dd1e564153d8007ebc4bb4e14250bde84e26a34")
  (package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
  (package! ox-asciidoc :pin "c8bc184f9088b76fdf1ce20e6e5d0a1588e1b327"))

;;ELISP
(when (featurep! :lang emacs-lisp)
  (package! emacs-inspector :recipe (:host github :repo "mmontone/emacs-inspector") :pin "8c555dfadb737bf2f05ff2786dbab2ac985775ef")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

;; MAIL
(when (featurep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "cdc2e141d8ecd59508a5cd50d6d02120073bf4f1"))

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
(package! pandoc-mode :pin "2a4e726a29d38e7c2379787cad619e5392ad2da0")
(package! centered-cursor-mode :pin "4093821cc9759ca5a3c6e527d4cc915fc3a5ad74")
(package! ssh-config-mode :pin "d560a0876a93ad4130baf33dae1b9405ad37a405")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! fix-word :pin "e967dd4ac98d777deeede8b497d6337634c06df4")
(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")
(package! ialign :pin "eca40b8b59ea713dba21b18f5b047a6c086b91dc")
(package! s :pin "08661efb075d1c6b4fa812184c1e5e90c08795a9")
(package! graphql-mode :pin "9740e4027bd9313697d5cac5caaa5b15626ab1da")
(package! vundo :pin "0b8538f4e4c2f25cb9d5b75cf064ba8dceca6145")

(when (featurep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
