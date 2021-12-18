;; -*- lexical-binding: t; -*-


(unpin! dired-git-info)
(unpin! code-review)

;; C++
(package! ninja-mode :recipe (:files ("*.el")) :pin "e5935b63757f3a788bc56d2c7afd9e390daf2f07")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "32bc21d4f0e011dbdb7dc1a9d1cd8651353f2943")

;; GIT
(when (featurep! :tools magit)
  (package! magit-imerge :pin "1ee213d7fa1536c86c128d09946b44ededbfac9c")
  (package! git-commit :pin "f766f68f78b982443850b2e4433d83e26971fdb0")
  (package! rigid-tabs :pin "eba84ceaba2e57e76ad2dfbb7a7154238a25d956")
  (package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099"))

;;ORG
(when (featurep! :lang org)
  (package! demo-it :pin "9cfa5c3f92a0dca7eebb1f1a2011643c9b009d26")
  (package! org-super-agenda :pin "fb5e2ef277bc811a3b061106c99e4c47b6b86f80")
  (package! doct :pin "c1919a4297e5479d3a22ded90095245317b29935")
  (package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
  (package! org-ql :pin "31aeb0a2505acf8044c07824888ddec7f3e529c1")
  (package! org-appear :pin "a4d10fc346ba14f487eb7aa95761b9295089ba55")
  (package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
  (package! ox-asciidoc :pin "d60ac439278cec214882f92c47bc16e0f43ae98e"))

;;ELISP
(package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a")

;; MAIL
(when (featurep! :email mu4e)
  (package! mu4e-maildirs-extension :pin "1167bc6e08996f866e73e9a02f563fd21ac317fd"))

(when (and (featurep! :email mu4e)
           (featurep 'xwidget-internal))
  (package! mu4e-views :pin "f3f454c7f92e8a9eecb5501af9ca81a547fd1841"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! expand-region :pin "95a773bd8f557cbd43d3b2dab2fa4417ec5927ab")
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

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))
