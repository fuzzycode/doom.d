;;; tools/ai/packages.el -*- lexical-binding: t; -*-

;; ;; gptel
(package! gptel :pin "0f65be08ead0c9bc882fad5a4dcb604448e366a6")
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick") :pin "018ff2be8f860a1e8fe3966eec418ad635620c38")
(package! gptel-agent :pin "99a8b940271fbe68cdfb7c2329d090dc4ef04b99")
;; (package! gptel-prompts :recipe (:host github :repo "jwiegley/gptel-prompts"))
(package! gptel-tool-library :recipe (:host github :repo "aard-fi/gptel-tool-library") :pin "baffc3b0d74a2b7cbda0d5cd6dd7726d6ccaca83")
(package! llm-tool-collection :recipe (:host github :repo "skissue/llm-tool-collection") :pin "a383ccf3df6c86684da77fb61ea4ebe67a21eedb")
(package! gptel-prompt-file :recipe (:host github :repo "fuzzycode/gptel-prompt-file"))

;; copilot
(package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el") :pin "125e0a4478ff1404880ea4e593f5e4ff0122cb83")
