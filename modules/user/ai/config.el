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

;; (defun +bl/gptel-mcp-register-tools ()
;;   "Register all mcp tools with gptel."
;;   (interactive)
;;   (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
;;     (mapcar #'(lambda (tool)
;;                 (apply #'gptel-make-tool
;;                        tool))
;;             tools)))

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
   :category "emacs"
   :include t)

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

  (set-popup-rule! "\\*Mcp-Hub\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil))


(use-package! gptel
  :commands (gptel gptel-send gptel-menu)
  :bind ("C-c RET" . #'gptel-send)
  :init (setq gptel-expert-commands t
              gptel-default-mode 'org-mode
              gptel-prompt-prefix-alist '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "## "))
              gptel-response-prefix-alist '((markdown-mode . "## ") (org-mode . "** *@assistant*\n") (text-mode . "### ")))
  (map! :leader :desc "Gptel" "RET" #'gptel-menu)
  (map! (:leader
         (:prefix "l"
                  (:prefix "g"
                   :desc "Add" "a" #'gptel-add
                   :desc "Open Chat" "g" #'gptel
                   :desc "Open Menu" "m" #'gptel-menu
                   :desc "Send" "s" #'gptel-send
                   :desc "Rewrite Region" "R" #'gptel-rewrite)
                  :desc "Select Session" "l" #'+bl/gptel-select-session)
         (:prefix "p"
          :desc "Open Agent" "A" #'+bl/open-project-agent-file)))
  :config

  ;; Make copilot with Claude the default
  (setq gptel-model 'claude-opus-4.5
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Add other providers
  (gptel-make-anthropic "Claude" :stream t :key +bl/anthropic-api-key)
  (gptel-make-gemini "Gemini" :stream t :key +bl/google-api-key)
  (gptel-make-openai "OpenAi" :stream t :key +bl/openai-api-key)

  (after! gptel-transient
    ;; Tuning the menu options
    (transient-suffix-put 'gptel-menu (kbd "-m") :key "M")
    (transient-suffix-put 'gptel-menu (kbd "-i") :key "I")
    (transient-suffix-put 'gptel-menu (kbd "-c") :key "L")
    (transient-suffix-put 'gptel-menu (kbd "-v") :key "V")
    (transient-suffix-put 'gptel-menu (kbd "-t") :key "T")
    (transient-suffix-put 'gptel-menu (kbd "-T") :key "C")
    (transient-suffix-put 'gptel-menu (kbd "=") :key "S")
    (transient-suffix-put 'gptel-menu (kbd "-n") :key "N")
    (transient-suffix-put 'gptel-menu (kbd "-b") :key "B")
    (transient-suffix-put 'gptel-menu (kbd "-f") :key "F")
    (transient-suffix-put 'gptel-menu (kbd "-R") :key "R"))

  ;; Configure behavior
  ;; (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'+bl/gptel-insert-response-properteis-h)
  (add-hook 'gptel-post-response-functions #'+bl/goto-empty-prompt-maybe-h)
  (add-hook 'gptel-save-state-hook #'+bl/gptel-mode-auto-h)
  (add-hook 'gptel-post-request-hook #'+bl/gptel-normal-state-after-send-h)
  (add-hook 'gptel-post-request-hook #'+bl/abort-completions-h)
  (add-hook 'gptel-mode-hook #'gptel--prettify-preset)

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

(use-package! ragmacs
  :after gptel)

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

;; (use-package! chatgpt-shell
;;   :defer t
;;   :init (setq shell-maker-history-path doom-data-dir
;;               chatgpt-shell-root-path doom-data-dir
;;               chatgpt-shell-anthropic-key +bl/anthropic-api-key
;;               chatgpt-shell-google-key +bl/google-api-key
;;               chatgpt-shell-openai-key +bl/openai-api-key)
;;   (map! :leader (:prefix "l"
;;                  :desc "ChatGPT" "c" #'chatgpt-shell))
;;   :config
;;   (set-popup-rule! (lambda (buf &rest _)
;;                      (with-current-buffer buf
;;                        (eq major-mode 'chatgpt-shell-mode)))
;;     :size 0.5 :side 'bottom :select t :quit t :ttl nil))

;; (use-package! dall-e-shell
;;   :defer t
;;   :init (setq dall-e-shell-openai-key +bl/openai-api-key)
;;   (map! :leader (:prefix "l"
;;                  :desc "DALL-E" "d" #'dall-e-shell))
;;   :config
;;   (set-popup-rule! "^\\*dall-e\\*$" :side 'bottom :size .5 :select t :quit 'current))

;; (use-package! ob-chatgpt-shell
;;   :when (modulep! :lang org)
;;   :defer t
;;   :hook (org-mode . ob-chatgpt-shell-setup))

;; (use-package! ob-dall-e-shell
;;   :when (modulep! :lang org)
;;   :defer t
;;   :hook (org-mode . ob-dall-e-shell-setup))
