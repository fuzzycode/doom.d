;;; tools/ai/autoload/gptel.el -*- lexical-binding: t; -*-

(defvar +bl/gptel-archive-directory
  (expand-file-name "gptel-archive/" (or (getenv "XDG_DATA_HOME") "~/.local/share/"))
  "Root directory for archived gptel conversations.")

;;;###autoload
(defun +bl/gptel-archive-subtree (&optional choose-location)
  "Archive the current level-1 subtree to a date-organized file.

The subtree containing point is moved to a file at:
  `+bl/gptel-archive-directory'/YYYY/MM/DD.org

With prefix argument CHOOSE-LOCATION, prompt for an existing
archive file to append to instead of using today's date.

This only works in buffers with `gptel-mode' enabled."
  (interactive "P")
  (unless (bound-and-true-p gptel-mode)
    (user-error "Not in a gptel buffer"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (save-excursion
    ;; Navigate to level 1 heading
    (org-back-to-heading t)
    (while (> (org-current-level) 1)
      (org-up-heading-safe)))
  ;; Now at level 1 heading - get the subtree
  (let* ((target-file (if choose-location
                          (+bl/gptel-archive--choose-file)
                        (expand-file-name
                         (format-time-string "%Y/%m/%d.org")
                         +bl/gptel-archive-directory)))
         (element (org-element-at-point))
         (begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (subtree-text (buffer-substring-no-properties begin end)))
    ;; Ensure directory exists
    (make-directory (file-name-directory target-file) t)
    ;; Append to archive file (ensure newline separation)
    (with-temp-buffer
      (when (file-exists-p target-file)
        (insert-file-contents target-file)
        (goto-char (point-max))
        ;; Ensure we start on a fresh line
        (unless (bolp)
          (insert "\n")))
      (goto-char (point-max))
      (insert subtree-text)
      (write-region (point-min) (point-max) target-file))
    ;; Remove from current buffer
    (delete-region begin end)
    (message "Archived to %s" target-file)))

(defun +bl/gptel-archive--choose-file ()
  "Prompt user to choose an existing archive file or enter a new path.
Returns an absolute path within `+bl/gptel-archive-directory'."
  (let* ((default-directory +bl/gptel-archive-directory)
         (existing-files (when (file-directory-p +bl/gptel-archive-directory)
                           (directory-files-recursively
                            +bl/gptel-archive-directory
                            "\\.org$")))
         (relative-files (mapcar (lambda (f)
                                   (file-relative-name f +bl/gptel-archive-directory))
                                 existing-files))
         (default-choice (format-time-string "%Y/%m/%d.org"))
         (choice (completing-read
                  "Archive to: "
                  (cons default-choice relative-files)
                  nil nil nil nil default-choice)))
    (expand-file-name choice +bl/gptel-archive-directory)))

;;;###autoload
(defun +bl/gptel-archive-browse ()
  "Browse gptel archive files.
Opens a file browser in the archive directory."
  (interactive)
  (unless (file-directory-p +bl/gptel-archive-directory)
    (user-error "Archive directory does not exist: %s" +bl/gptel-archive-directory))
  (find-file +bl/gptel-archive-directory))

;;;###autoload
(defun +bl/gptel-archive-search (&optional initial)
  "Search gptel archive using ripgrep.
INITIAL is the optional initial search input."
  (interactive)
  (unless (file-directory-p +bl/gptel-archive-directory)
    (user-error "Archive directory does not exist: %s" +bl/gptel-archive-directory))
  (consult-ripgrep +bl/gptel-archive-directory initial))

;;;###autoload
(defun +bl/gptel-archive-find ()
  "Find a file in the gptel archive."
  (interactive)
  (unless (file-directory-p +bl/gptel-archive-directory)
    (user-error "Archive directory does not exist: %s" +bl/gptel-archive-directory))
  (let ((default-directory +bl/gptel-archive-directory))
    (call-interactively #'find-file)))

;;; Chat history functionality for gptel
(defvar +bl/gptel-history-directory
  (expand-file-name "gptel-history/" (or (getenv "XDG_DATA_HOME") "~/.local/share/"))
  "Root directory for saved gptel conversation files.")

(defvar +bl/gptel-name-prompt
  "Generate a short, descriptive filename (2-5 words, lowercase, hyphens between words) for this conversation.
Output ONLY the filename, no extension, no explanation, no quotes."
  "System prompt for generating conversation names.")

(defvar-local +bl/gptel-generated-name nil
  "Buffer-local variable storing the AI-generated name for this conversation.")

;;;###autoload
(defun +bl/gptel-generate-name ()
  "Generate a descriptive name for the current gptel conversation using AI.
The generated name is stored in `+bl/gptel-generated-name' and used
when saving the buffer. Also renames the buffer if it's not visiting a file."
  (interactive)
  (unless (bound-and-true-p gptel-mode)
    (user-error "Not in a gptel buffer"))
  (let ((content (buffer-substring-no-properties
                  (point-min)
                  (min (point-max) (+ (point-min) 2000))))) ;; First ~2000 chars
    (when (string-empty-p (string-trim content))
      (user-error "Buffer is empty, nothing to name"))
    (message "Generating name...")
    (gptel-request content
      :system +bl/gptel-name-prompt
      :callback
      (lambda (response info)
        (if (not (stringp response))
            (message "Failed to generate name: %s" (plist-get info :status))
          (let* ((name (string-trim response))
                 ;; Clean up the name: lowercase, hyphens, no special chars
                 (clean-name (replace-regexp-in-string
                              "[^a-z0-9-]" ""
                              (replace-regexp-in-string
                               "[ _]+" "-"
                               (downcase name)))))
            (when (string-empty-p clean-name)
              (setq clean-name "untitled"))
            (setq +bl/gptel-generated-name clean-name)
            ;; Rename buffer if not visiting a file
            (unless (buffer-file-name)
              (rename-buffer (concat "*" clean-name "*") t))
            (message "Generated name: %s" clean-name)))))))

;;;###autoload
(defun +bl/gptel-history-save ()
  "Save the current gptel buffer to the history directory.
Uses `+bl/gptel-generated-name' if available, otherwise asks user
if they want to save with a timestamp. If declined, aborts.
Only acts on gptel-mode buffers not visiting a file."
  (interactive)
  (unless (bound-and-true-p gptel-mode)
    (user-error "Not in a gptel buffer"))
  (when (buffer-file-name)
    ;; Already visiting a file, use normal save
    (call-interactively #'save-buffer)
    (cl-return-from +bl/gptel-history-save))
  (let* ((name (or +bl/gptel-generated-name
                   (if (y-or-n-p "No name found. Save with timestamp? ")
                       (format-time-string "%Y%m%d-%H%M%S")
                     (user-error "Save aborted"))))
         (extension (pcase major-mode
                      ('org-mode ".org")
                      ('markdown-mode ".md")
                      (_ ".txt")))
         (filename (concat name extension))
         (target-file (expand-file-name filename +bl/gptel-history-directory)))
    ;; Ensure unique filename
    (when (file-exists-p target-file)
      (let ((counter 1))
        (while (file-exists-p target-file)
          (setq target-file (expand-file-name
                             (concat name "-" (number-to-string counter) extension)
                             +bl/gptel-history-directory))
          (cl-incf counter))))
    ;; Ensure directory exists
    (make-directory +bl/gptel-history-directory t)
    ;; Set the filename and save
    (set-visited-file-name target-file t)
    (save-buffer)
    (message "Saved to %s" target-file)))

(defun +bl/gptel-history--write-contents ()
  "Hook function to handle saving gptel buffers not visiting a file.
Added to `write-contents-functions' in gptel-mode buffers."
  (when (and (bound-and-true-p gptel-mode)
             (not (buffer-file-name)))
    (+bl/gptel-history-save)
    t)) ;; Return t to indicate the buffer has been saved

;;;###autoload
(defun +bl/gptel-history-enable-h ()
  "Enable automatic saving to history for the current gptel buffer."
  (add-hook 'write-contents-functions #'+bl/gptel-history--write-contents nil t))

;;;###autoload
(defun +bl/gptel-history-browse ()
  "Browse saved gptel conversation files."
  (interactive)
  (unless (file-directory-p +bl/gptel-history-directory)
    (user-error "History directory does not exist: %s" +bl/gptel-history-directory))
  (find-file +bl/gptel-history-directory))

;;;###autoload
(defun +bl/gptel-history-find ()
  "Find and open a saved gptel conversation.
Uses completing-read to select from available history files."
  (interactive)
  (unless (file-directory-p +bl/gptel-history-directory)
    (user-error "History directory does not exist: %s" +bl/gptel-history-directory))
  (let* ((files (directory-files +bl/gptel-history-directory nil "\\.\\(org\\|md\\|txt\\)$"))
         (choice (completing-read "Open conversation: " files nil t)))
    (when choice
      (find-file (expand-file-name choice +bl/gptel-history-directory)))))

;;;###autoload
(defun +bl/gptel-history-search (&optional initial)
  "Search gptel conversation history using ripgrep.
INITIAL is the optional initial search input."
  (interactive)
  (unless (file-directory-p +bl/gptel-history-directory)
    (user-error "History directory does not exist: %s" +bl/gptel-history-directory))
  (consult-ripgrep +bl/gptel-history-directory initial))

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

;; ;;;###autoload
;; (defun +bl/gptel-define-word (&optional arg-or-word)
;;   "Define word possibly provided by ARG-OR-WORD.
;; If a word is provided as argument, that is used.
;; If given the universal argument, the user is prompted for a word.
;; If no word is provided, the word at point is used. "
;;   (interactive "P")
;;   (let ((word (cond
;;                ((stringp arg-or-word) arg-or-word)
;;                ((and arg-or-word (called-interactively-p 'any)) (read-string "Word to Define: "))
;;                (t (or (thing-at-point 'word t)
;;                       (user-error "No word found to lookup")))))
;;         (context (or (thing-at-point 'sentence t) "")))
;;     (gptel-request word
;;       :context context
;;       :system +bl/gptel-default-define-word-prompt
;;       :callback (lambda (response info)
;;                   (if (not response)
;;                       (Message "Lookup failed with error %s" (plist-get info :status))
;;                     (with-current-buffer (get-buffer-create +bl/gptel-define-word-buffer)
;;                       (let ((inhibit-read-only t))
;;                         (erase-buffer)
;;                         (insert response)
;;                         (goto-char (point-min)))
;;                       (special-mode)
;;                       (visual-line-mode)
;;                       (display-buffer (current-buffer))))))))

;; ;;;###autoload
;; (defun +bl/gptel-lookup (prompt)
;;   "Ask the agent a question(PROMPT) and get a response in a dedicated buffer."
;;   (interactive (list (read-string (format "Ask Agent%s: " (+bl/gptel-backend-stringify)) nil +bl/gptel-lookup--history)))
;;   (when (string-empty-p prompt) (user-error "A prompt is required to ask the agent"))
;;   (gptel-request prompt
;;     :callback (lambda (response info)
;;                 (if (not response)
;;                     (message "Lookup failed with error %s" (plist-get info :status))
;;                   (with-current-buffer (get-buffer-create +bl/gptel-lookup-buffer)
;;                     (let ((inhibit-read-only t))
;;                       (erase-buffer)
;;                       (insert response)
;;                       (goto-char (point-min)))
;;                     (special-mode)
;;                     (visual-line-mode)
;;                     (display-buffer (current-buffer)))))))

;; ;;;###autoload
;; (defun +bl/gptel-review-code (bounds &optional prompt)
;;   "Review code using gptel.

;; BOUNDS will either be the active region or the whole buffer.
;; By providing the universal argument the PROMPT can be tailored
;; for each individual request."
;;   (interactive
;;    (list
;;     (if (use-region-p)
;;         (cons (region-beginning) (region-end))
;;       (cons (point-min) (point-max)))
;;     (and current-prefix-arg
;;          (read-string (format "Agent Directive%s: " (+bl/gptel-backend-and-model-maybe))
;;                       +bl/gptel-default-review-prompt))))
;;   (gptel-request
;;       (buffer-substring-no-properties (car bounds) (cdr bounds))
;;     :system (or prompt +bl/gptel-default-review-prompt)
;;     :context (cons (set-marker (make-marker) (car bounds))
;;                    (set-marker (make-marker) (cdr bounds)))
;;     :callback (lambda (response info)
;;                 (if (not response)
;;                     (message "Review call failed with error %s" (plist-get info :status))
;;                   (with-current-buffer (get-buffer-create +bl/gptel-lookup-buffer)
;;                     (let ((inhibit-read-only t))
;;                       (erase-buffer)
;;                       (insert response)
;;                       (goto-char (point-min)))
;;                     (special-mode)
;;                     (visual-line-mode)
;;                     (display-buffer (current-buffer)))))))

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
(defun +bl/gptel-goto-response-start-h (beg _end)
  "Move point to the beginning of the response."
  (when beg
    (goto-char beg)))

;;;###autoload
(defun +bl/abort-completions-h ()
  "Disable any lingering completion windows."
  (when (featurep 'company)
    (company-abort))
  (when (featurep 'corfu)
    (corfu-quit)))


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
(defun +bl/gptel-select-session ()
  "Select an existing gptel session or create a new one.
Lists all buffers with `gptel-mode' enabled and offers to create a new session."
  (interactive)
  (require 'gptel)
  (let* ((backend (default-value 'gptel-backend))
         (new-buffer-name (generate-new-buffer-name
                           (concat "*" (gptel-backend-name backend) "*")))
         (buffer-name
          (read-buffer
           "Select or create gptel session: "
           new-buffer-name
           nil
           (lambda (buf-name)
             "Predicate to filter gptel buffers and allow new buffer creation."
             (when (consp buf-name) (setq buf-name (car buf-name)))
             (let ((buf (get-buffer buf-name)))
               (or (null buf)  ; Allow creating new buffers
                   (buffer-local-value 'gptel-mode buf)))))))
    (gptel buffer-name nil nil t)))


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
(defun +bl/goto-empty-prompt-maybe-h (_ _)
  "After response hook to move point to the empty prompt."
  (+bl/goto-empty-prompt-maybe))

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
(defun +bl/get-tools (server-name)
  "Return all tools for SERVER-NAME."
  (if-let ((connection (gethash server-name mcp-server-connections)))
      (mcp--tools connection)
    '()))

;;;###autoload
(defun +bl/get-tool-name (tool)
  "Get the name of TOOL."
  (plist-get tool :name))

;;;###autoload
(defun +bl/read-only-github-tool-p (tool)
  "Predicate to identify if TOOL is a read-only GitHub tool."
  (when-let ((name (plist-get tool :name)))
    (or
     (string-match-p "list_" name)
     (string-match-p "get_" name)
     (string-match-p "search_" name)
     (string-match-p "_read" name))))

;;;###autoload
(defun +bl/context-buffer-p (buf)
  "Return non-nil if BUF is a buffer that can be included in a context."
  (when-let ((name (buffer-name buf)))
    (not
     (or
      ;; Filter out buffers with names starting with space (internal)
      (string-prefix-p " " name)
      ;; Filter out *foo* style buffers
      (and (string-prefix-p "*" name)
           (string-suffix-p "*" name))
      ;; Filter out buffers visiting hidden files
      (when-let ((file (buffer-file-name buf)))
        (string-prefix-p "." (file-name-nondirectory file)))))))

;;;###autoload
(defun +bl/visible-buffer-list ()
  "Return a list of visible buffers in the current frame.
Only includes buffers currently displayed in windows.
Filters out:
- Buffers visiting hidden files (starting with '.')
- Internal/uninteresting buffers (typically *foo* style buffers)
- Buffers with names starting with a space"
  (let ((buffers (mapcar #'window-buffer
                         (window-list nil 'no-minibuf))))
    (cl-remove-if
     (lambda (buf)
       (not (+bl/context-buffer-p buf)))
     (cl-remove-duplicates buffers))))

;;;###autoload
(defun +bl/workspace-buffer-list ()
  "Return a list of buffers in the current workspace.
Filters out:
- Buffers visiting hidden files (starting with '.')
- Internal/uninteresting buffers (typically *foo* style buffers)
- Buffers with names starting with a space"
  (let ((buffers (if (and (bound-and-true-p persp-mode)
                          (not *persp-pretend-switched-off*))
                     (safe-persp-buffers (get-current-persp))
                   (buffer-list))))
    (cl-remove-if
     (lambda (buf)
       (not (+bl/context-buffer-p buf)))
     buffers)))

;; ;;;###autoload
;; (defun +bl/open-project-agent-file ()
;;   "Open agent.org file in the project root if it exists.
;;    Does nothing if not in a project or if the file doesn't exist."
;;   (interactive)
;;   (if-let ((project-root (doom-project-root)))
;;       (let ((agent-file (expand-file-name "agent.org" project-root)))
;;         (if (file-exists-p agent-file)
;;             (find-file agent-file)
;;           (message "No agent.org file found in project root: %s" project-root)))
;;     (message "Not in a project")))
