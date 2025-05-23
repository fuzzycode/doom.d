;;; tools/ai/autoload/tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/gptel-tool-print-message (text)
  "A tool to output TEXT as message to user."
  (message "%s" text)
  (format "Message Sent: %s" text))

;;;###autoload
(defun +bl/gptel-tool-read-documentation (symbol)
  "Read the documentation for SYMBOL.
SYMBOL can either be a function or a variable."
  (message "Using tool +bl/gptel-tool-read-documentation: %s" symbol)
  (let ((sym (intern-soft symbol)))
    (if sym
        (cond
         ((fboundp sym)
          (documentation sym))
         ((boundp sym)
          (documentation-property sym 'variable-documentation))
         (t
          (format "%s was not a variable or function. No documentation available" symbol)))
      (format "No symbol named %s found" symbol))))


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
  (let ((cmd (cond
              ((eq type 'command) 'apropos-command)
              ((eq type 'function) 'apropos-function)
              ((eq type 'variable) 'apropos-variable)
              (t 'apropos))))
    (with-output-to-string (funcall cmd pattern do-all))))

;;;###autoload
(defun +bl/gptel-tool-get-function-implementation (function-name)
  "Return the implementation of a function given its NAME.
Return nil if FUNCTION-NAME is not a function or cannot be found."
  (when-let* ((sym (if (symbolp function-name)
                       function-name
                     (intern-soft function-name)))
              ((fboundp sym))
              (func (symbol-function sym)))
    (with-temp-buffer
      (let ((print-level nil)
            (print-length nil))
        (pp func (current-buffer))
        (buffer-string)))))

;;;###autoload
(defun +bl/gptel-tool-get-function-source (function-name)
  "Return the source code of a function given its NAME.
Returns nil if FUNCTION-NAME is not a function or cannot be found.
This retrieves the actual source code without expanding macros."
  (when-let* ((sym (if (symbolp function-name)
                       function-name
                     (intern-soft function-name)))
              ((fboundp sym)))
    (condition-case nil
        (pcase-let ((`(,buf . ,pos) (find-function-noselect sym)))
          (with-current-buffer buf
            (save-excursion
              (if pos
                  (progn
                    (goto-char pos)
                    ;; Find the start of the definition
                    (when (re-search-backward "^\\s-*(\\(def\\|cl-def\\)" nil t)
                      (let ((start (point))
                            (sexp-end))
                        ;; Find the end by reading the whole sexp
                        (forward-sexp 1)
                        (setq sexp-end (point))
                        ;; Return the complete function definition
                        (buffer-substring-no-properties start sexp-end))))
                nil))))
      (error nil))))
