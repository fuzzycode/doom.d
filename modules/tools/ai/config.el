;;; tools/ai/config.el -*- lexical-binding: t; -*-

(defvar +bl/google-api-key
  (lambda () (auth-source-pick-first-password :host "Google AI API Key" :user "password")))
(defvar +bl/anthropic-api-key
  (lambda () (auth-source-pick-first-password :host "Claude API" :user "password")))
(defvar +bl/openai-api-key
  (lambda () (auth-source-pick-first-password :host "OpenAI API Key" :user "password")))


(map! (:leader
       (:prefix ("l" . "llms")
                (:prefix ("g" . "gptel")))
       (:prefix "c" (:prefix ("l" . "llms")))))

(use-package! mcp
  :defer t)

(defun +bl/gptel-setup-tools ()
  "Setup the list of available tools provided to LLMs."
  (gptel-make-tool
   :name "print_message"
   :function #'+bl/gptel-tool-print-message
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
                 :type string
                 :description "The text to send to the messages buffer"))
   :category "Emacs")

  (gptel-make-tool
   :name "read_documentation"
   :function #'+bl/gptel-tool-read-documentation
   :description "Read the documentation for a given function or variable"
   :args (list '(:name "name"
                 :type string
                 :description "The name of the function or variable whose documentation is to be retrieved"))
   :category "Emacs"))

(use-package! gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind ("C-c RET" . #'gptel-send)
  :init (setq gptel-default-mode 'org-mode
              gptel-expert-commands t
              gptel-prompt-prefix-alist '((markdown-mode . "##") (org-mode . "** *Prompt*: ") (text-mode . "##"))
              gptel-response-prefix-alist '((markdown-mode . "###") (org-mode . "*** *Response*: ") (text-mode . "###")))
  (map! :map gptel-mode-map
        :nmg "G" #'+bl/goto-empty-prompt-maybe)
  (map! (:leader (:prefix "l"
                          (:prefix "g"
                           :desc "Ask" "a" #'+bl/gptel-lookup
                           :desc "Open Chat" "g" #'gptel
                           :desc "Open Menu" "G" #'gptel-menu
                           :desc "Review" "r" #'+bl/gptel-review-code
                           :desc "Rewrite Region" "R" #'gptel-rewrite))))
  :config
  ;; Make copilot with sonnet 3.7 the default
  (setq gptel-model 'claude-3.7-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Add providers
  (gptel-make-anthropic "Claude" :stream t :key +bl/anthropic-api-key)
  (gptel-make-gemini "Gemini" :stream t :key +bl/google-api-key)
  (gptel-make-openai "OpenAi" :stream t :key +bl/openai-api-key)

  ;; Configure behavior
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions #'+bl/gptel-insert-response-properteis-h)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response 100)
  (add-hook 'gptel-save-state-hook #'+bl/gptel-mode-auto-h)
  (add-hook 'gptel-post-request-hook #'+bl/gptel-normal-state-after-send-h)

  (when (eq gptel-default-mode 'org-mode)
    (add-hook 'org-ctrl-c-ctrl-c-hook #'+bl/gptel-ctr-c-ctr-c-h))

  ;; Add the tools
  (+bl/gptel-setup-tools)

  ;; Catch the gptel tooling windows
  (set-popup-rule! "\\*gptel-\\(lookup\\|review\\)\\*" :size 0.4 :side 'bottom :select t :quit 'current :ttl nil)

  ;; Catch all gptel chat buffers
  (set-popup-rule!
    (lambda (buf &rest _)
      (with-current-buffer buf
        (and gptel-mode
             (memq major-mode '(org-mode markdown-mode)))))
    :size 0.4 :side 'right :select t :quit nil :ttl nil :modeline t))

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
