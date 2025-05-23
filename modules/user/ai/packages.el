;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; gptel
(package! gptel :pin "93c98514d0daf3d10a116e03a96042a8e263f5d3")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

;; copilot
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")) :pin "fe3f51b636dea1c9ac55a0d5dc5d7df02dcbaa48")

;; chatgpt-shell
(package! chatgpt-shell :pin "dad8ba7cf5531098713d70e12c577a2a49c55ca2")
(package! dall-e-shell :pin "efec43ab3338e59f12165110e6c6c957345f6257")

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el") :pin "78ef40ee9e1eaa6ee6f5ba26da853c0a2c5f8426")

(when (modulep! :lang org)
  (package! ob-chatgpt-shell :pin "754ddf54a99bd98427c91a6c7374757026f8bd45")
  (package! ob-dall-e-shell :pin "9c052b7d55b33d7e76747a4dde0eb130a17c33e1"))
