;;; tools/ai/autoload/copilot.el -*- lexical-binding: t; -*-

(defvar +bl/chat-history-dir
  "~/Documents/chat-history/"
  "The default directory where chats are saved and loaded from")

(defvar +bl/gptel-lookup--history nil
  "Store the history of previous quick lookups that was done during the session")

(defvar +bl/gptel-default-review-prompt
  "You are an expert coder. Please review the following code and provide feedback on its correctness, efficiency, and style.
If you find any issues, please suggest improvements or alternatives. If the code is correct, please confirm that it is correct.
If you provide any suggestions or improvements, please explain why they are better than the original code
and include code examples to explain the suggested changes."
  "The default prompt when asking gptel to review the region or buffer.")

(defvar +bl/gptel-default-define-word-prompt
  "You are a dictionary. Please provide the definition of the word. Give 3 examples of how to use the word in a sentence."
  "The system prompt used when asking gptel to define a word.")

(defvar +bl/gptel-define-word-buffer "*gptel-word*"
  "The name of the buffer used for gptel word lookups.")

(defvar +bl/gptel-lookup-buffer "*gptel-lookup*"
  "The name of the buffer used for gptel lookups.")

(defvar +bl/gptel-review-buffer "*gptel-review*"
  "The name of the buffer used for gptel reviews.")

;;;###autoload
(defun +bl/gptel-backend-and-model-maybe ()
  "Return the backend and model for gptel if they are set."
  (let ((backend (if (boundp 'gptel-backend) (aref gptel-backend 1) ""))
        (model (if (boundp 'gptel-model) gptel-model "")))
    (when (and backend model)
      (cons backend (symbol-name model)))))

;;;###autoload
(defun +bl/gptel-backend-stringify ()
  "Return backend and model as a string."
  (let ((backend-and-model (+bl/gptel-backend-and-model-maybe)))
    (if backend-and-model
        (format "%s:%s" (car backend-and-model) (cdr backend-and-model))
      "n/a")))

;;;###autoload
(defun +bl/gptel-define-word (&optional arg-or-word)
  "Define word possibly provided by ARG-OR-WORD.
If a word is provided as argument, that is used.
If given the universal argument, the user is prompted for a word.
If no word is provided, the word at point is used. "
  (interactive "P")
  (let ((word (cond
               ((stringp arg-or-word) arg-or-word)
               ((and arg-or-word (called-interactively-p 'any)) (read-string "Word to Define: "))
               (t (or (thing-at-point 'word t)
                      (user-error "No word found to lookup")))))
        (context (or (thing-at-point 'sentence t) "")))
    (gptel-request word
      :context context
      :system +bl/gptel-default-define-word-prompt
      :callback (lambda (response info)
                  (if (not response)
                      (Message "Lookup failed with error %s" (plist-get info :status))
                    (with-current-buffer (get-buffer-create +bl/gptel-define-word-buffer)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert response)
                        (goto-char (point-min)))
                      (special-mode)
                      (visual-line-mode)
                      (display-buffer (current-buffer))))))))

;;;###autoload
(defun +bl/gptel-lookup (prompt)
  "Ask the agent a question(PROMPT) and get a response in a dedicated buffer."
  (interactive (list (read-string (format "Ask Agent%s: " (+bl/gptel-backend-stringify)) nil +bl/gptel-lookup--history)))
  (when (string-empty-p prompt) (user-error "A prompt is required to ask the agent"))
  (gptel-request prompt
    :callback (lambda (response info)
                (if (not response)
                    (message "Lookup failed with error %s" (plist-get info :status))
                  (with-current-buffer (get-buffer-create +bl/gptel-lookup-buffer)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert response)
                      (goto-char (point-min)))
                    (special-mode)
                    (visual-line-mode)
                    (display-buffer (current-buffer)))))))

;;;###autoload
(defun +bl/gptel-review-code (bounds &optional prompt)
  "Review code using gptel.

BOUNDS will either be the active region or the whole buffer.
By providing the universal argument the PROMPT can be tailored
for each individual request."
  (interactive
   (list
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (cons (point-min) (point-max)))
    (and current-prefix-arg
         (read-string (format "Agent Directive%s: " (+bl/gptel-backend-and-model-maybe))
                      +bl/gptel-default-review-prompt))))
  (gptel-request
      (buffer-substring-no-properties (car bounds) (cdr bounds))
    :system (or prompt +bl/gptel-default-review-prompt)
    :context (cons (set-marker (make-marker) (car bounds))
                   (set-marker (make-marker) (cdr bounds)))
    :callback (lambda (response info)
                (if (not response)
                    (message "Review call failed with error %s" (plist-get info :status))
                  (with-current-buffer (get-buffer-create +bl/gptel-lookup-buffer)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert response)
                      (goto-char (point-min)))
                    (special-mode)
                    (visual-line-mode)
                    (display-buffer (current-buffer)))))))

;;;###autoload
(defun +bl/has-prop-line ()
  "Check if the current line has a property line."
  (and (save-excursion
         (goto-char (point-min))
         (looking-at ".*-\\*-"))))

;;;###autoload
(defun +bl/gptel-mode-auto-h ()
  "Ensures that the gptel-mode local variable is
added and true in the current file."
  (let ((enable-local-variables t)
        (inhibit-read-only t))
    (save-excursion
      (save-restriction
        (when (+bl/has-prop-line)
          (modify-file-local-variable-prop-line 'eval nil 'delete))
        (add-file-local-variable-prop-line 'eval
                                           '(and (require 'gptel nil t) (fboundp 'gptel-mode) (gptel-mode 1)))))))

;;;###autoload
(defun +bl/gptel-normal-state-after-send-h ()
  "A hook to automatically enter normal state after a request has been sent."
  (when (featurep 'evil)
    (evil-normal-state)))

;;;###autoload
(defun +bl/gptel-browse-chats ()
  (interactive)
  (if (file-directory-p +bl/chat-history-dir)
      (dired +bl/chat-history-dir)
    (message "%s does not exist" +bl/chat-history-dir)))

;;;###autoload
(defun +bl/gptel-insert-response-properteis-h (begin end)
  "Inserts the response meta properties for the response between BEGIN and END."
  (unless (eq begin end) ;; If the call fails they are the same
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (org-back-to-heading t)
        (let ((time (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (org-entry-put begin "Created" time))
        (when-let ((backend-and-model (+bl/gptel-backend-and-model-maybe)))
          (org-entry-put begin "Backend" (car backend-and-model))
          (org-entry-put begin "Model" (cdr backend-and-model)))
        (widen)))))

;;;###autoload
(defun +bl/point-in-prompt-p (prefix)
  "Return non-nil if point is on a heading starting
with PREFIX or in text directly under it.
Does not return true if point is in a sub-heading."
  (save-excursion
    (let ((initial-level (org-current-level)))
      (and (org-back-to-heading t)
           (let ((heading-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                 (prompt-level (org-current-level)))
             (and (string-prefix-p prefix heading-line)
                  (or (null initial-level)
                      (= initial-level prompt-level))))))))

;;;###autoload
(defun +bl/gptel-ctr-c-ctr-c-h ()
  "If inside a prompt this will cause C-C C-c to send the request."
  (when (bound-and-true-p gptel-mode)
    (let ((prefix (gptel-prompt-prefix-string)))
      (if (+bl/point-in-prompt-p prefix)
          (progn
            (org-end-of-subtree)
            (gptel-send)
            t)
        nil))))

;;;###autoload
(defun +bl/gptel-find-last-prefix-match (prefix)
  "Find the last match for PREFIX in the current buffer."
  (save-excursion
    (let ((regexp (concat "^" (regexp-quote prefix))))
      (goto-char (point-max))
      (if (re-search-backward regexp nil t)
          (match-end 0)
        (message "No match found for prefix: %s" prefix))) ))

;;;###autoload
(defun +bl/goto-empty-prompt-maybe ()
  "Move point to the empty prompt if it exists."
  (interactive)
  (when-let ((pos (+bl/gptel-find-last-prefix-match (gptel-prompt-prefix-string))))
    (goto-char pos)))

;;;###autoload
(defun +bl/get-ollama-models ()
  "Get a list of installed Ollama models (first column names only)."
  (interactive)
  (if (executable-find "ollama")
      (let* ((output (shell-command-to-string "ollama list"))
             (models (+bl/parse-first-column output)))
        (if (called-interactively-p 'any)
            (message "Installed Ollama models: %s" models)
          models))
    (user-error "Ollama executable not found in path")))

;;;###autoload
(defun +bl/parse-first-column (text)
  "Parse TEXT and return a list of items from the first column, excluding header.
TEXT is assumed to be in a tabular format with columns separated by whitespace."
  (let ((lines (split-string text "\n" t))
        result)
    (when (> (length lines) 1)
      (setq lines (cdr lines))
      (dolist (line lines)
        (when (not (string-empty-p (string-trim line)))
          (let ((first-column (car (split-string line "\\s-\\s-+" t))))
            (push (string-trim first-column) result)))))
    (nreverse result)))

;;;###autoload
(defun +bl/open-project-agent-file ()
  "Open agent.org file in the project root if it exists.
   Does nothing if not in a project or if the file doesn't exist."
  (interactive)
  (if-let ((project-root (doom-project-root)))
      (let ((agent-file (expand-file-name "agent.org" project-root)))
        (if (file-exists-p agent-file)
            (find-file agent-file)
          (message "No agent.org file found in project root: %s" project-root)))
    (message "Not in a project")))
