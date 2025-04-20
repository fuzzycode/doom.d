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

