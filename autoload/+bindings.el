;;; ../Development/GitHub/dotfiles/doom.d/autoload/+bindings.el -*- lexical-binding: t; -*-

(require 's)

;;;###autoload
(defun +bl/format-command-name (name)
  "Format NAME by removing hyphens and adding capitalization."
  (format "◂%s" (s-capitalize (s-trim (replace-regexp-in-string "--?" " " name)))))

;; TODO(Björn Larsson): Simplify these functions and remove the need to duplicate the regexps

;;;###autoload
(defun +bl/beautify-evil (value)
  "Beautify evil function name in VALUE."
  (+bl/format-command-name (replace-regexp-in-string "\\+?evil\\(?:nc\\|em\\)?[:/-]\\(?:a-\\|motion-\\)?\\(.+\\)" "\\1" value)))

;;;###autoload
(defun +bl/beautify-org (value)
  "Beautify org function name in VALUE."
  (+bl/format-command-name (replace-regexp-in-string "\\(?:\\?\\|consult-\\)?org[:/-]\\(.+\\)" "\\1" value)))

;;;###autoload
(defun +bl/beautify-doom (value)
  "Beautify org function name in VALUE."
  (+bl/format-command-name (replace-regexp-in-string "\\+\\(.+\\)[:/]\\(.+\\)" "\\1 \\2" value)))
