;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; ;; gptel
(package! gptel :pin "195f240a61336d64bda950f6201db3eaec9ea060")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick") :pin "018ff2be8f860a1e8fe3966eec418ad635620c38")
(package! gptel-agent :pin "99a8b940271fbe68cdfb7c2329d090dc4ef04b99")
;; (package! gptel-prompts :recipe (:host github :repo "jwiegley/gptel-prompts"))
(package! gptel-tool-library :recipe (:host github :repo "aard-fi/gptel-tool-library") :pin "baffc3b0d74a2b7cbda0d5cd6dd7726d6ccaca83")
(package! llm-tool-colection :recipe (:host github :repo "skissue/llm-tool-collection") :pin "a383ccf3df6c86684da77fb61ea4ebe67a21eedb")
(package! gptel-prompt-file :recipe (:host github :repo "fuzzycode/gptel-prompt-file"))

;; copilot
(package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
;; (package! shell-maker)

;; ;; chatgpt-shell
;; (package! chatgpt-shell :pin "c5b9394fed338eb5bb129590aa29edb14f6d9ba7")
;; (package! dall-e-shell :pin "428125f9fa8578703a9ca85d173b2cc9a3eb16b9")

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el") :pin "963b4af6ce743fbb6224f61bb61f05de1c37f511")

;; (when (modulep! :lang org)
;;   (package! ob-chatgpt-shell :pin "0e592d19528f8f3283a93e0e2844299e9ea21fcc")
;;   (package! ob-dall-e-shell :pin "9c052b7d55b33d7e76747a4dde0eb130a17c33e1"))
