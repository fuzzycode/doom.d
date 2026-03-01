;;; tools/ai/config.el -*- lexical-binding: t; -*-

(defvar +bl/google-api-key
  (lambda () (auth-source-pick-first-password :host "Google AI API Key" :user "password")))
(defvar +bl/anthropic-api-key
  (lambda () (auth-source-pick-first-password :host "Claude API" :user "password")))
(defvar +bl/openai-api-key
  (lambda () (auth-source-pick-first-password :host "OpenAI API Key" :user "password")))

(defvar +bl/ollama-host "localhost:11434"
  "The host for the ollama server.")

;; Add a place to store AI key bindings
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
  ;; (gptel-make-tool
  ;;  :function #'+bl/gptel-tool-apropos-search
  ;;  :name "apropos_search"
  ;;  :description "Search Emacs for functions, variables, and other symbols matching a pattern. Returns formatted results as text."
  ;;  :args (list '(:name "pattern"
  ;;                :type string
  ;;                :description "A string containing a regular expression to match against symbol names.")
  ;;              '(:name "do_all"
  ;;                :type boolean
  ;;                :description "If true, search all symbols, not just user-facing ones like commands and variables."
  ;;                :optional t)
  ;;              '(:name "type"
  ;;                :type string
  ;;                :enum ["command" "function" "variable"]
  ;;                :description "Restrict search to a specific type of symbol."
  ;;                :optional t))
  ;;  :category "emacs"
  ;;  :include t)

  (gptel-make-tool
   :function #'+bl/get-project-root
   :name "get_project_root"
   :description "Get the root directory of the current project."
   :args (list '(:name "path"
                 :type string
                 :description "The file or directory path to find the project root for. Defaults to the current buffer's directory."
                 :optional t))
   :category "emacs"
   :include t))

(use-package! mcp
  :after gptel
  :init (setq mcp-hub-servers '())
  (map! :leader (:prefix "l"
                 :desc "MCP Hub" "M" #'mcp-hub))

  :config
  (require 'gptel-integrations nil t)

  ;; (gptel-mcp-connect nil '+bl/gptel-mcp-register-tools nil)

  (set-popup-rule! "\\*Mcp-Hub\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil))


(use-package! gptel
  :commands (gptel gptel-send gptel-menu)
  :bind ("C-c RET" . #'gptel-send)
  :init (setq gptel-expert-commands t
              gptel-default-mode 'org-mode
              gptel-include-reasoning " *llm-thoughts*"
              gptel-prompt-prefix-alist '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "## "))
              gptel-response-prefix-alist '((markdown-mode . "## ") (org-mode . "** *@assistant*\n") (text-mode . "### ")))
  (map! :map gptel-mode-map
        :n "G" #'+bl/goto-empty-prompt-maybe)
  (map! :leader :desc "Gptel" "RET" #'gptel-menu)
  (map! (:leader
         (:prefix "l"
                  (:prefix "g"
                   :desc "Search History" "s" #'+bl/gptel-history-search
                   :desc "Find History" "f" #'+bl/gptel-history-find
                   :desc "Browse History" "b" #'+bl/gptel-history-browse
                   :desc "Search Archive" "S" #'+bl/gptel-archive-search
                   :desc "Find Archive" "F" #'+bl/gptel-archive-find
                   :desc "Browse Archive" "B" #'+bl/gptel-archive-browse
                   :desc "Add" "a" #'gptel-add
                   :desc "Open Chat" "g" #'gptel
                   :desc "Open Menu" "m" #'gptel-menu
                   :desc "Rewrite Region" "R" #'gptel-rewrite)
                  :desc "Select Session" "l" #'+bl/gptel-select-session)
))
  :config

  ;; Make copilot with Claude the default
  (setq gptel-model 'claude-opus-4.5
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Add other providers
  (gptel-make-anthropic "Claude" :stream t :key +bl/anthropic-api-key)
  (gptel-make-gemini "Gemini" :stream t :key +bl/google-api-key)
  (gptel-make-openai "OpenAi" :stream t :key +bl/openai-api-key)


  ;; Configure behavior
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'+bl/gptel-insert-response-properteis-h)
  (add-hook 'gptel-post-response-functions #'+bl/gptel-goto-response-start-h)
  (add-hook 'gptel-save-state-hook #'+bl/gptel-mode-auto-h)
  (add-hook 'gptel-pre-response-hook #'+bl/gptel-normal-state-after-send-h)
  (add-hook 'gptel-post-request-hook #'+bl/abort-completions-h)
  (add-hook 'gptel-mode-hook #'gptel--prettify-preset)


  (when (eq gptel-default-mode 'org-mode)
    (add-hook 'org-ctrl-c-ctrl-c-hook #'+bl/gptel-ctr-c-ctr-c-h))

  (when (boundp 'persp-mode)
    (add-hook 'gptel-mode-hook (lambda () (persp-add-buffer (current-buffer)))))


  (+bl/gptel-setup-tools)

  ;; Catch the gptel tooling windows
  (set-popup-rule! "\\*gptel-\\(lookup\\|review\\|word\\)\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil)

  ;; Catch all gptel chat buffers
  (set-popup-rule!
    (lambda (buf &rest _)
      (with-current-buffer buf
        (and gptel-mode
             (memq major-mode '(org-mode markdown-mode)))))
    :size 0.4 :side 'right :select t :quit nil :ttl nil :modeline t))

(use-package! gptel-prompt-file
  :after gptel
  :config (push '(file . (gptel-prompt-from-file-dynamic)) gptel-directives))

;; Community driven set of tools for gptel
(use-package! gptel-tool-library
  :after gptel
  :init (setq gptel-tool-library-use nil ;; Disable all tools by default
              gptel-tool-library-use-maybe-safe nil
              gptel-tool-library-use-unsafe nil)
  :config
  ;; Other available modules '("bbdb" "gnus" "os")
  (dolist (module '("buffer" "elisp" "emacs"))
    (gptel-tool-library-load-module module)))

(use-package! llm-tool-collection
  :after gptel
  :config (mapc (apply-partially #'apply #'gptel-make-tool)
                (llm-tool-collection-get-all)))

(use-package! gptel-agent
  :after gptel
  :init (setq gptel-agent-dirs (list (expand-file-name "agents" doom-user-dir)))
  :config (gptel-agent-update))

;; Provides copilot based completions
(use-package! copilot
  :hook (prog-mode . +bl/try-enable-copilot)
  :init (setq copilot-idle-delay 0.5)
  (map! (:leader (:prefix "t" :desc "Copilot" "a" #'+bl/try-enable-copilot)))
  :config
  ;;Tailor how and when Copilot is active
  (add-to-list 'copilot-enable-predicates #'+bl/enable-copilot-p)
  :bind (:map copilot-completion-map
              ("M-RET" . #'copilot-accept-completion)
              ("S-M-RET" . #'copilot-accept-completion-by-word)
              ("C-M-RET" . #'copilot-accept-completion-by-line)))

;; (use-package! gptel-prompts
;;   :after gptel
;;   :init (setq gptel-prompts-directory (expand-file-name "prompts" doom-user-dir))
;;   :config
;;   (gptel-prompts-update)
;;   ;; Ensure prompts are updated if prompt files change
;;   (gptel-prompts-add-update-watchers))

(use-package! gptel-quick
  :after gptel
  :commands (gptel-quick)
  :init (after! embark
          (keymap-set embark-general-map "?" #'gptel-quick))
  (map! (:leader (:prefix "l" (:prefix "g" :desc "Quick" "q" #'gptel-quick)))))

(after! mcp
  ;; Define toolsets to be used in presets
  ;; (defconst +bl/time-tools '("mcp-time"))
  ;; (defconst +bl/web-tools '("WebFetch" "WebSearch"))
  ;; (defconst +bl/buffer-tools '("view_buffer" "list_buffers" "buffer_search"))
  ;; (defconst +bl/file-system-tools '("read_file" "list_directory" "view_file" "glob" "grep" "ls"))
  ;; (defconst +bl/project-tools '("get_project_root"))
  ;; (defconst +bl/developer-tools (append +bl/time-tools +bl/web-tools +bl/buffer-tools +bl/file-system-tools +bl/project-tools))
  ;; (defconst +bl/lisp-tools '("elisp-fuzzy-match" "elisp-describe-symbol" "elisp-function-doc" "elisp-variable-doc" "defun-region"))
  (defconst +bl/github-read-only-tools (mapcar #'+bl/get-tool-name (seq-filter #'+bl/read-only-github-tool-p (+bl/get-tools "github"))))


  (gptel-make-preset 'json
    :description "Inline preset to specify JSON schema on the fly"
    :pre (lambda ()
           (setq-local gptel--schema
                       (buffer-substring-no-properties
                        (point) (point-max)))
           (delete-region (point) (point-max)))
    :include-reasoning nil)

  (gptel-make-preset 'default
    :tools '()
    :description "A plain and default preset with no tools or special behavior."
    :system 'default)

  ;;   (gptel-make-preset 'base
  ;;                      :description "Base preset that others will inherit from"
  ;;                      :system "If you find that you are in an org-mode buffer, make any headings you create start at level 3.")


  ;;   (gptel-make-preset 'developer
  ;;                      :tools +bl/developer-tools
  ;;                      :parents '(base)
  ;;                      :description "Base for all developer presets"
  ;;                      :system '(:prepend "You are an expert software developer.
  ;; Provide accurate and efficient code snippets in response to user requests.
  ;; When asked to write code, ensure it is well-structured, follows best practices, and includes comments for clarity.
  ;; If the user provides a specific programming language or framework, tailor your responses accordingly.
  ;; Always prioritize readability and maintainability in your code examples.
  ;; You should not ask to provide any further steps unless explicitly asked."))

  (gptel-make-preset 'vb
    :description "A preset that provides access to visible buffers"
    :context '(:eval (+bl/visible-buffer-list)))

  (gptel-make-preset 'ab
    :description "A preset that provides access to all open buffers"
    :context '(:eval (+bl/workspace-buffer-list)))

  ;;   (gptel-make-preset 'project
  ;;                      :tools (append +bl/developer-tools +bl/github-read-only-tools)
  ;;                      :description "A project oriented preset"
  ;;                      :system 'file)

  ;;   (gptel-make-preset 'lisp-project
  ;;                      :description "A Lisp project oriented preset"
  ;;                      :tools (append +bl/developer-tools +bl/lisp-tools +bl/github-read-only-tools)
  ;;                      :system 'file)

  ;;   (gptel-make-preset 'lisper
  ;;                      :tools (append +bl/developer-tools +bl/lisp-tools)
  ;;                      :parents '(developer)
  ;;                      :description "Developer preset tailored for Lisp languages"
  ;;                      :system '(:prepend "You are an expert Lisp developer.
  ;; Provide accurate and efficient
  ;; Lisp. New code should follow the code standards of existing code."))

  ;;   (gptel-make-preset 'github-read-only
  ;;                      :description "Provide read-only GitHub tools"
  ;;                      :pre (lambda () (gptel-mcp-connect '("github") 'sync))
  ;;                      :tools +bl/github-read-only-tools)

  ;;   (gptel-make-preset 'github
  ;;                      :description "Provide all github tools"
  ;;                      :pre (lambda () (gptel-mcp-connect '("github") 'sync))
  ;;                      :tools '(:append ("mcp-github")))

  (gptel-make-preset 'explain
    :description "A preset that comes with a tutor tuned system prompt"
    :system (gptel-prompt-from-file-dynamic (expand-file-name "prompts/explain.md" (file-name-directory (buffer-file-name)))))

  (gptel-make-preset 'explore
    :description "A preset that comes with an exploratory tuned system prompt"
    :system (gptel-prompt-from-file-dynamic (expand-file-name "prompts/explore.md" (file-name-directory (buffer-file-name)))))

  (gptel-make-preset 'inline
    :description "A preset intended for inline responses"
    :system " Output only the requested content. No explanations, no preamble, no commentary, no markdown code fences unless explicitly requested. Your response will be inserted directly into a document."
    :include-reasoning nil)

  ;; GPT models
  (gptel-make-preset 'gpt-4.1
    :description "GPT-4.1 via Copilot"
    :backend "Copilot"
    :model 'gpt-4.1)

  (gptel-make-preset 'gpt-5
    :description "GPT-5 via Copilot"
    :backend "Copilot"
    :model 'gpt-5)

  (gptel-make-preset 'gpt-5-mini
    :description "GPT-5 Mini via Copilot"
    :backend "Copilot"
    :model 'gpt-5-mini)

  (gptel-make-preset 'o4-mini
    :description "o4-mini (reasoning) via Copilot"
    :backend "Copilot"
    :model 'o4-mini)

  ;; Claude models
  (gptel-make-preset 'sonnet
    :description "Claude Sonnet 4 via Copilot"
    :backend "Copilot"
    :model 'claude-sonnet-4)

  ;; Gemini models
  (gptel-make-preset 'gemini-3
    :description "Gemini 3 Pro via Copilot"
    :backend "Copilot"
    :model 'gemini-3-pro-preview)

  (gptel-make-preset 'gemini-2.5
    :description "Gemini 2.5 Pro via Copilot"
    :backend "Copilot"
    :model 'gemini-2.5-pro))
