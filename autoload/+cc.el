;;; ../Development/GitHub/dotfiles/doom.d/autoload/+cc.el -*- lexical-binding: t; -*-


;;;###autoload
(defun inside-string-p ()
  (nth 3 (syntax-ppss)))

;;;###autoload
(defun inside-include-p ()
  (save-excursion
    (or (and (inside-string-p)
             (looking-at-p ".+\"")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+\".+")
             )
        (and (looking-at-p ".+>")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+<.+")))))

;;;###autoload
(defun disable-flycheck-in-includes-a (result)
  (and result
       (not (inside-include-p))))

;;;###autoload
(advice-add #'flyspell-generic-progmode-verify :filter-return #'disable-flycheck-in-includes-a)
