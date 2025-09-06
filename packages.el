;; -*- lexical-binding: t; -*-


(package! magit-todos :disable t)
(package! ccls :disable t)
(package! rtags :disable t)


;; C++
(package! ninja-mode :recipe (:files ("*.el"))  :pin "573c3aaedc6e90e9a8954bb70a24e079af7df390")
(package! ff-c-style :recipe (:host github :repo "fuzzycode/ff-c-style"))
(package! tspew :recipe (:host github :repo "jefftrull/tspew" :files ("*.el"))) ;;Improve template error messages

(package! shader-mode :pin "fe5a1982ba69e4a98b834141a46a1908f132df15") ;; This is also a part of the csharp module but I only want this package from it
(package! metal-mode :recipe (:host github :repo "masfj/metal-mode") :pin "686ad916f53589d59797613c1050922dd424d492")
(package! cmake-font-lock :pin "a6038e916bcca807ae695f7d7e5c300c3f38f415")
(package! platformio-mode :pin "f4fd8932995a8aed80eab14e54232010c2889012")


;;ORG
(when (modulep! :lang org)
  (package! org-auto-tangle :pin "56e7afc35e4a6321d11c535600c287dbb1a90bc3")
  (package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf") :pin "080cfd2ed630a6739633b07a8ab6b896a1b5ef4a")
  (package! ob-mermaid :pin "372c2d91d3cdba5da9f7ac23e7bce7a0b3b46862")
  (package! demo-it :pin "e399fd7ceb73caeae7cb50b247359bafcaee2a3f")
  (package! org-super-agenda :pin "fb20ad9c8a9705aa05d40751682beae2d094e0fe")
  (package! org-make-toc :pin "5f0f39b11c091a5abf49ddf78a6f740252920f78")
  (package! org-ql :pin "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca")
  (package! ox-gfm :pin "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb")
  (package! ox-asciidoc :pin "0be7969ea78e4ac5a7be8ec1c08ac74e0c5c563d"))

;;ELISP
(when (modulep! :lang emacs-lisp)
  (package! inspector :pin "49b106f38e75b290911ecc3e3e582c2fc8cda792")
  (package! eval-sexp-fu :pin "36d2fe3bcf602e15ca10a7f487da103515ef391a"))

(when (modulep! :emacs dired)
  (package! dired+ :pin "bc49d6283f89b80d32bdb47d52ff6c3302b61564"))

(package! swedish-holidays :recipe (:host github :repo "fuzzycode/swedish-holidays"))
(package! lang-mode :recipe (:host github :repo "fuzzycode/lang-mode"))
(package! visual-regexp-steroids :pin "a6420b25ec0fbba43bf57875827092e1196d8a9e")
(package! visual-regexp :pin "48457d42a5e0fe10fa3a9c15854f1f127ade09b5")
(package! sort-words :pin "7b6e108f80237363faf7ec28b2c58dec270b8601")
(package! smart-backspace :pin "a10ec44ff325ec8c4c98b1a6e44e89e60a9aa4ac")
(package! open-junk-file :pin "558bec7372b0fed4c4cb6074ab906535fae615bd")
(package! pandoc-mode :pin "d7f6fa119bb0e883cfd615009d197e4b87916033")
(package! centered-cursor-mode :pin "67ef719e685407dbc455c7430765e4e685fd95a9")
(package! ssh-config-mode :pin "d0596f5fbeab3d2c3c30eb83527316403bc5b2f7")
(package! smart-newline :pin "0553a9e4be7188352de1a28f2eddfd28e7436f94")
(package! hardhat :pin "908cb130be3d56921a3687a00b974ba5eef3a11f")
(package! string-inflection :pin "91d6c991abcf9cfb2af403368962a9c7983235b4")
(package! ialign :pin "dfa5c38ae2f7665596c9a24bedc8e4bf4b554920")
(package! deadgrep :pin "edb1957d0d6033698c6784686b27508034003fa0")
(package! wgrep-deadgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")
(package! copy-as-format :pin "b9f6f725ca9701c5a02bfb479573fdfcce2e1e30")
(package! auth-source-1password :pin "7bb8ad3507c58cc642b2ebbd7e57a91efab80e14")
(package! mermaid-ts-mode  :pin "973e442cbed980cf51afc256c90ef133c4d02141")
(package! catppuccin-theme :pin "d070e926504b81dd9b8cdbf90604b5268e7e17b5")
(package! chezmoi :pin "1389782f8c0780c7e66f8e77b10345ba1f4eabae")
(package! obsidian :pin "0b31775d5da1dfd3d1ffcf9fa05908a3ba26ed15")
(package! graphql-mode)
(package! verb)

(when (modulep! :lang web)
  (package! yarn-mode :pin "8239d4dc7d8a52fa1e3fa81bd32c904a359fcfc1"))

(when (featurep :system 'macos)
  (package! reveal-in-osx-finder :pin "5710e5936e47139a610ec9a06899f72e77ddc7bc"))

;; Add evil packages
(when (modulep! :editor evil)
  (package! evil-textobj-line :pin "9eaf9a5485c2b5c05e16552b34632ca520cd681d"))


;; Allow local config packages
(load! "~/.packages.local.el" "" t)
