;;; private/spelling/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun inside-string-p ()
  (nth 3 (syntax-ppss)))

;;;###autoload
(defun inside-include-p ()
  (save-excursion
    (or (and (inside-string-p)
             (looking-at-p ".+\"")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+\".+"))
        (and (looking-at-p ".+>")
             (looking-back "^[[:blank:]]*#include[[:blank:]]+<.+")))))

;;;###autoload
(defun disable-flycheck-in-includes-a (result)
  (and result
       (not (inside-include-p))))

;;;###autoload
(advice-add #'flyspell-generic-progmode-verify :filter-return #'disable-flycheck-in-includes-a)

;;;###autoload
(defun +spelling//detect-ispell-args (&optional run-together)
  "Generate a list of the correct ispell arguments"
  (let ((args '("--sug-mode=ultra" "--lang=en" "--dont-tex-check-comments")))
    (when run-together
      (setq args (append args '("--camel-case"))))
    args))

;;;###autoload
(defun +spelling//setup-text-mode ()
  "Ignore CamelCase in text mode"
  (setq-local ispell-extra-args (+spelling//detect-ispell-args))
  (require 'wucuo)
  (wucuo-start))

;;;###autoload
(defun +spelling//adjust-correct-word-args-a (orig-func &rest args)
  "Disables the CamelCase option when correcting a word"
  (let ((ispell-extra-args (+spelling//detect-ispell-args)))
    (ispell-kill-ispell t)
    (apply orig-func args)
    (ispell-kill-ispell t)))

;;;###autoload
(advice-add 'ispell-word :around #'+spelling//adjust-correct-word-args-a)
;;;###autoload
(advice-add 'flyspell-auto-correct-word :around #'+spelling//adjust-correct-word-args-a)

;; Taken from Doom flyspell module

(defvar +spell--flyspell-predicate-alist nil
  "TODO")

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  "TODO"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes) +spell--flyspell-predicate-alist)
    (add-to-list '+spell--flyspell-predicate-alist (cons mode predicate))))

;;;###autoload
(defun +spell-init-flyspell-predicate-h ()
  "TODO"
  (when-let (pred (assq major-mode +spell--flyspell-predicate-alist))
    (setq-local flyspell-generic-check-word-predicate (cdr pred))))
