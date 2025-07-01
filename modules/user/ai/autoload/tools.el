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
