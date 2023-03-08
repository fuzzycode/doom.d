;;; ../Development/GitHub/dotfiles/doom.d/autoload/+flyspell.el -*- lexical-binding: t; -*-
;;;###if (modulep! :checkers spell)

;;;###autoload
(defun +bl/inside-string-p ()
  "Test if point is inside a string or not."
  (nth 3 (syntax-ppss (point))))

;;;###autoload
(defun +bl/inside-include-p ()
  "Test if point is inside an include statement or not."
  (save-excursion
    (goto-char (1- (point)))
    (or (and (+bl/inside-string-p)
             (looking-at-p ".+\"")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+\".+" (line-beginning-position)))
        (and (looking-at-p ".+>")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+<.+" (line-beginning-position))))))

;;;###autoload
(defun +bl/cc-flyspell-predicate-p ()
  "Extend normal flyspell predicate to disable spellchecking in includes."
  (and (flyspell-generic-progmode-verify)
       (not (+bl/inside-include-p))))
