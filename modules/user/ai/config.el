;;; tools/ai/config.el -*- lexical-binding: t; -*-

(defvar +bl/google-api-key
  (lambda () (auth-source-pick-first-password :host "Google AI API Key" :user "password")))
(defvar +bl/anthropic-api-key
  (lambda () (auth-source-pick-first-password :host "Claude API" :user "password")))
(defvar +bl/openai-api-key
  (lambda () (auth-source-pick-first-password :host "OpenAI API Key" :user "password")))

(defvar +bl/ollama-host "localhost:11434"
  "The host for the ollama server.")

(map! (:leader
       (:prefix ("l" . "llms")
                (:prefix ("g" . "gptel")))
       (:prefix "c" (:prefix ("l" . "llms")))))

(defun +bl/gptel-mcp-register-tools ()
  "Register all mcp tools with gptel."
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))

(defun +bl/gptel-setup-tools ()
  "Setup the list of available tools provided to LLMs."
  (gptel-make-tool
   :function #'+bl/gptel-tool-apropos-search
   :name "apropos_search"
   :description "Search Emacs for functions, variables, and other symbols matching a pattern. Returns formatted results as text."
   :args (list '(:name "pattern"
                 :type string
                 :description "A string containing a regular expression to match against symbol names.")
               '(:name "do_all"
                 :type boolean
                 :description "If true, search all symbols, not just user-facing ones like commands and variables."
                 :optional t)
               '(:name "type"
                 :type string
                 :enum ["command" "function" "variable"]
                 :description "Restrict search to a specific type of symbol."
                 :optional t))
   :category "Emacs"
   :include t))

(use-package! mcp
  :defer t
  :init
  (setq mcp-hub-servers
        '(("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
          ("elisp" . (:command "~/.emacs.d/.local/straight/repos/mcp-server-lib.el/emacs-mcp-stdio.sh"
                      :args ("--init-function=elisp-dev-mcp-enable" "--stop-function=elisp-dev-mcp-disable")))
          ("mermaid" . (:command "npx" :args ("-y" "mcp-mermaid")))))

  (map! :leader (:prefix "l"
                 :desc "MCP Hub" "M" #'mcp-hub))

  (add-hook 'doom-first-file-hook #'mcp-server-lib-start 80) ;; Make sure elisp-dev-mcp is started before mcp-hub
  (add-hook 'doom-first-file-hook #'mcp-hub-start-all-server 90)

  (after! gptel
    (+bl/gptel-mcp-register-tools))

  :config
  (set-popup-rule! "\\*Mcp-Hub\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil))

(use-package! elisp-dev-mcp
  :after mcp)

(use-package! gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind ("C-c RET" . #'gptel-send)
  :init (setq gptel-default-mode 'org-mode
              gptel-expert-commands t
              gptel-org-branching-context t
              gptel-prompt-prefix-alist '((markdown-mode . "##") (org-mode . "*@user*\n") (text-mode . "##"))
              gptel-response-prefix-alist '((markdown-mode . "###") (org-mode . "*@assistant*\n") (text-mode . "###")))
  (map! :map gptel-mode-map
        :n "G" #'+bl/goto-empty-prompt-maybe)
  (map! (:leader
         :desc "Gptel" "RET" #'gptel-menu
         (:prefix "l"
                  (:prefix "g"
                   :desc "Add" "a" #'gptel-add
                   :desc "Ask" "A" #'+bl/gptel-lookup
                   :desc "Define Word" "d" #'+bl/gptel-define-word
                   :desc "Open Chat" "g" #'gptel
                   :desc "Open Menu" "m" #'gptel-menu
                   :desc "Send" "s" #'gptel-send
                   :desc "Review" "r" #'+bl/gptel-review-code
                   :desc "Rewrite Region" "R" #'gptel-rewrite))
         (:prefix "p"
          :desc "Open Agent" "A" #'+bl/open-project-agent-file)))
  :config
  ;;Load MCP integration
  (require 'gptel-integrations nil t)

  ;; Make copilot with Claude the default
  (setq gptel-model 'claude-opus-4
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Add providers
  (gptel-make-anthropic "Claude" :stream t :key +bl/anthropic-api-key)
  (gptel-make-gemini "Gemini" :stream t :key +bl/google-api-key)
  (gptel-make-openai "OpenAi" :stream t :key +bl/openai-api-key)
  (when (executable-find "ollama")
    (gptel-make-ollama "Ollama" :stream t :host +bl/ollama-host :models (+bl/get-ollama-models)))

  ;; Configure behavior
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'+bl/gptel-insert-response-properteis-h)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response 100)
  (add-hook 'gptel-save-state-hook #'+bl/gptel-mode-auto-h)
  (add-hook 'gptel-post-request-hook #'+bl/gptel-normal-state-after-send-h)

  (when (eq gptel-default-mode 'org-mode)
    (add-hook 'org-ctrl-c-ctrl-c-hook #'+bl/gptel-ctr-c-ctr-c-h))

  ;; Add the local tools
  (+bl/gptel-setup-tools)

  ;; Catch the gptel tooling windows
  (set-popup-rule! "\\*gptel-\\(lookup\\|review\\\word\\)\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil)

  ;; Catch all gptel chat buffers
  (set-popup-rule!
    (lambda (buf &rest _)
      (with-current-buffer buf
        (and gptel-mode
             (memq major-mode '(org-mode markdown-mode)))))
    :size 0.4 :side 'right :select t :quit nil :ttl nil :modeline t))

(use-package! gptel-prompts
  :after gptel
  :init (setq gptel-prompts-directory (expand-file-name "prompts" doom-user-dir))
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))

(use-package! gptel-aibo
  :after (gptel flycheck)
  :init (map! (:leader (:prefix "l" (:prefix "g" :desc "Aibo Summon" "s" #'gptel-aibo-summon)))))

(use-package! gptel-quick
  :after gptel
  :commands (gptel-quick)
  :init (after! embark
          (keymap-set embark-general-map "?" #'gptel-quick))
  (map! (:leader (:prefix "l" (:prefix "g" :desc "Quick" "q" #'gptel-quick)))))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :init (setq copilot-idle-delay 0.5)
  (map! (:leader (:prefix "t" :desc "Copilot" "a" #'copilot-mode)))
  :config
  ;;Tailor how and when Copilot is active
  (add-to-list 'copilot-enable-predicates #'+bl/enable-copilot-p)
  :bind (:map copilot-completion-map
              ("M-RET" . #'copilot-accept-completion)
              ("S-M-RET" . #'copilot-accept-completion-by-word)
              ("C-M-RET" . #'copilot-accept-completion-by-line)))

(use-package! chatgpt-shell
  :defer t
  :init (setq shell-maker-history-path doom-data-dir
              chatgpt-shell-root-path doom-data-dir
              chatgpt-shell-anthropic-key +bl/anthropic-api-key
              chatgpt-shell-google-key +bl/google-api-key
              chatgpt-shell-openai-key +bl/openai-api-key)
  (map! :leader (:prefix "l"
                 :desc "ChatGPT" "c" #'chatgpt-shell))
  :config
  (set-popup-rule! (lambda (buf &rest _)
                     (with-current-buffer buf
                       (eq major-mode 'chatgpt-shell-mode)))
    :size 0.5 :side 'bottom :select t :quit t :ttl nil))

(use-package! dall-e-shell
  :defer t
  :init (setq dall-e-shell-openai-key +bl/openai-api-key)
  (map! :leader (:prefix "l"
                 :desc "DALL-E" "d" #'dall-e-shell))
  :config
  (set-popup-rule! "^\\*dall-e\\*$" :side 'bottom :size .5 :select t :quit 'current))

(use-package! ob-chatgpt-shell
  :when (modulep! :lang org)
  :defer t
  :hook (org-mode . ob-chatgpt-shell-setup))

(use-package! ob-dall-e-shell
  :when (modulep! :lang org)
  :defer t
  :hook (org-mode . ob-dall-e-shell-setup))

(use-package! eca
  :defer t
  :init (map! :leader (:prefix "l" :desc "ECA" "e" #'eca))
  (map! :map eca-chat-mode-map
        :n "q" #'quit-window
        :n [escape] #'kill-current-buffer))
