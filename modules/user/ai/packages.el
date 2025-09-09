;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; gptel
(package! gptel :pin "dd5e71734cd3ec3953cfc3a6b611f066e40ba9a0")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! gptel-aibo)
(package! elisp-dev-mcp)
(package! gptel-prompts :recipe (:host github :repo "jwiegley/gptel-prompts"))

;; copilot
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")) :pin "4f51b3c21c42756d09ee17011201ea7d6e18ff69")

;; chatgpt-shell
(package! chatgpt-shell :pin "c5b9394fed338eb5bb129590aa29edb14f6d9ba7")
(package! dall-e-shell :pin "428125f9fa8578703a9ca85d173b2cc9a3eb16b9")

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el") :pin "5f06a78fe74f58888f115bf30838ede5e013f4af")

(when (modulep! :lang org)
  (package! ob-chatgpt-shell :pin "0e592d19528f8f3283a93e0e2844299e9ea21fcc")
  (package! ob-dall-e-shell :pin "9c052b7d55b33d7e76747a4dde0eb130a17c33e1"))

;; ECA
(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
