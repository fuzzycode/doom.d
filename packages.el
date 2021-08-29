;; -*- lexical-binding: t; -*-

;; C++
(package! ninja-mode :pin "e90dfd3c7528b9c620eab29121a3591af7bf035e")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style") :pin "32bc21d4f0e011dbdb7dc1a9d1cd8651353f2943")
(package! sourcetrail :pin "b8d5557aa565ae979622312576db20515f65f977")

;; GIT
(package! magit-imerge :pin "cf3b4646aa0205e8d7f47e45165fe6403d6440f5")
(package! git-commit :pin "8406b084a31c9f4f331fb9846938deffdbe740e2")
(package! gitignore-mode :pin "433e1c57a63c88855fc41a942e29d7bc8c9c16c7")
(package! gitconfig-mode :pin "433e1c57a63c88855fc41a942e29d7bc8c9c16c7")
(package! gitattributes-mode :pin "433e1c57a63c88855fc41a942e29d7bc8c9c16c7")
(package! rigid-tabs :pin "eba84ceaba2e57e76ad2dfbb7a7154238a25d956")
(package! gitignore-templates :pin "d28cd1cec00242b688861648d36d086818b06099")

;;ORG
(package! demo-it :pin "9cfa5c3f92a0dca7eebb1f1a2011643c9b009d26")
(package! org-super-agenda :pin "a5557ea4f51571ee9def3cd9a1ab1c38f1a27af7")
(package! doct :pin "c7c8687ae8a7f1230732eaebc89ea668b4f7a37d")
(package! org-make-toc :pin "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa")
(package! org-ql :pin "94f9e6f3031b32cf5e2149beca7074807235dcb0")
(package! org-appear :pin "a1aa8496f2fd61305e43e03e6eeee2ff92aa9e24")
(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
(package! ox-asciidoc :pin "efb74df1179702e19ce531f84993ac5b5039075f")

;;ELISP
(package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a")

;; MAIL
;; (when (featurep! :email mu4e)
;;   (package! mu4e-maildirs-extension)
;;   (package! mu4e-alert))

;; (when (and (featurep! :email mu4e)
;;            (featurep 'xwidget-internal))
;;   (package! mu4e-views :recipe (:host github :repo "lordpretzel/mu4e-views")))

(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! expand-region :pin "95a773bd8f557cbd43d3b2dab2fa4417ec5927ab")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! winum :pin "c5455e866e8a5f7eab6a7263e2057aff5f1118b9")
(package! pandoc-mode :pin "39167ff0e9293b4632cf162a32c9d0b6990a371d")
(package! avy :pin "e92cb37457b43336b765630dbfbea8ba4be601fa")
(package! centered-cursor-mode :pin "4093821cc9759ca5a3c6e527d4cc915fc3a5ad74")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")

(when IS-MAC
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (featurep! :editor evil)
  (package! evil-surround :pin "3bd73794ee5a760118042584ef74e2b6fb2a1e06"))
