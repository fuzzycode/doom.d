;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; gptel
(package! gptel :pin "db4e5c7ea39f7d58d8ca4856c98a6df71b1e18bf")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

;; copilot
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")) :pin "6c8ad4e4b65f5eec026325db2ac5a0aaedd8b22c")

;; chatgpt-shell
(package! chatgpt-shell :pin "51e0523d6c7abeb5b99ac93b0af033d28acc36b2")
(package! dall-e-shell :pin "efec43ab3338e59f12165110e6c6c957345f6257")

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el") :pin "d0f3b5e53c1eded4619e537881e69c4f6d5a2a94")

(when (modulep! :lang org)
  (package! ob-chatgpt-shell :pin "754ddf54a99bd98427c91a6c7374757026f8bd45")
  (package! ob-dall-e-shell :pin "9c052b7d55b33d7e76747a4dde0eb130a17c33e1"))
