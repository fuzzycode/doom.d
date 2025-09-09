;;; tools/ai/autoload/tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/gptel-tool-apropos-search (pattern &optional do-all type)
  "Search for Emacs symbols matching PATTERN and return results as text.
This function wraps Emacs apropos functionality for use with gptel.

PATTERN is a string containing a regexp to match.
Optional DO-ALL, if non-nil, finds all symbols, not just commands,
variables, etc.
Optional TYPE restricts search to a specific type: \='command,
\='function, or \='variable.

Returns a formatted string with the search results."
  (with-temp-buffer
    (with-no-warnings
      (let ((standard-output (current-buffer))
            (apropos-sort-by-scores t))
        ;; Call the appropriate apropos function based on TYPE
        (cond
         ((eq type 'command) (apropos-command pattern do-all nil))
         ((eq type 'function) (apropos-function pattern do-all nil))
         ((eq type 'variable) (apropos-variable pattern do-all nil))
         (t (apropos pattern do-all)))

        ;; Return the collected output as a string
        (let ((result (buffer-string)))
          (if (string-empty-p result)
              (format "No results found for pattern: %s" pattern)
            result))))))

;;;###autoload
(defun +bl/gptel-tool-find-functions-with-keyword (keywords)
  "Find functions containing all KEYWORDS in their name or docstring."
  (interactive "sEnter keyword: ")
  (let ((results '()))
    (mapatoms
     (lambda (symbol)
       (when (fboundp symbol)
         (let ((name (symbol-name symbol))
               (doc (ignore-errors (documentation symbol))))
           (when (or (cl-every #'(lambda (keyword)
                                   (string-match-p (regexp-quote keyword) name))
                               keywords)
                     (and doc (cl-every #'(lambda (keyword)
                                            (string-match-p (regexp-quote keyword) doc))
                                        keywords)))
             (push symbol results))))))
    (if results
        (with-temp-buffer
          (insert (format "Functions containing '%s':\n\n" keywords))
          (dolist (func (sort results (lambda (a b)
                                        (string< (symbol-name a)
                                                 (symbol-name b)))))
            (insert (format "%-40s %s\n"
                            (symbol-name func)
                            (or (car (split-string (or (documentation func) "")
                                                   "\n"))
                                "No documentation"))))
          (buffer-string))
      (format "No functions found containing '%s'" keyword))))

;;;###autoload
(defun +bl/gptel-tool-get-function-docstring (name)
  "Return the documentation for a given function NAME."
  (let ((sym (intern-soft name)))
    (and sym
         (fboundp sym)
         (documentation sym))))

;;;###autoload
(defun +bl/gptel-tool-extract-function-source (function-name)
  "Extract and return the source code of a callable FUNCTION-NAME, if possible."
  (when (fboundp (intern function-name))
    (let ((source))
      (condition-case err
          ;; `find-function-noselect' returns the buffer and position.
          (let ((buf-pos (find-function-noselect (intern function-name))))
            (when buf-pos
              (let ((buffer (car buf-pos))
                    (pos (cdr buf-pos)))
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char pos)
                    (let ((start (point)))
                      ;; Move to the end of the function definition.
                      (end-of-defun)
                      ;; Extract source code.
                      (setq source (buffer-substring-no-properties start (point)))))))))
        (error
         ;; Handle errors gracefully by printing a message.
         (message "Error finding source for %s: %s" function-name (error-message-string err))))
      source)))

;;;###autoload
(defun +bl/gptel-tool-describe-variable (variable-name)
  "Return a plain text description of VARIABLE-NAME with its properties.
VARIABLE-NAME can be a string or symbol."
  (let* ((sym (if (stringp variable-name)
                  (intern variable-name)
                variable-name))
         (output '()))
    (if (not (boundp sym))
        (format "Variable '%s' is not bound." sym)
      ;; Build text output
      (push (format "Variable: %s\n" sym) output)
      (push (format "Value: %S\n" (symbol-value sym)) output)

      (when-let ((doc (documentation-property sym 'variable-documentation)))
        (push (format "Documentation:\n%s\n" doc) output))

      (when (local-variable-p sym)
        (push "Buffer Local: yes\n" output))

      (when-let ((custom-type (get sym 'custom-type)))
        (push (format "Custom Type: %S\n" custom-type) output))

      (when-let ((standard-value (get sym 'standard-value)))
        (push (format "Standard Value: %S\n" (car standard-value)) output))

      (when (get sym 'safe-local-variable)
        (push "Safe as File Local: yes\n" output))

      (when (get sym 'risky-local-variable)
        (push "Risky Local Variable: yes\n" output))

      (when-let ((alias (indirect-variable sym)))
        (unless (eq alias sym)
          (push (format "Alias for: %s\n" alias) output)))

      (string-join (nreverse output) ""))))
