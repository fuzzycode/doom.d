;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; gptel
(package! gptel :pin "9349d90ffbb9cc067ae1553485f7391199c5ca74")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

;; copilot
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")) :pin "3e36a9825616e756f16c7c6a33f4edd5fb466260")

;; chatgpt-shell
(package! chatgpt-shell :pin "5fc6c333b3bc91fba41871f5a89217804a66eabd")
(package! dall-e-shell :pin "efec43ab3338e59f12165110e6c6c957345f6257")

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el"))

(when (modulep! :lang org)
  (package! ob-chatgpt-shell)
  (package! ob-dall-e-shell))
