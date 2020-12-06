;; -*- lexical-binding: t; -*-

(map! :localleader
        :map emacs-lisp-mode-map
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
          :desc "Instrument Defun ON" "f" #'+emacs-lisp/edebug-instrument-defun-on
          :desc "Instrument Defun OFF" "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
          :desc "Eval Buffer" "b" #'eval-buffer
          :desc "Eval Defun" "d" #'eval-defun
          :desc "Eval Last s-exp" "e" #'eval-last-sexp
          :desc "Eval Region" "r" #'eval-region
          :desc "Load Library" "l" #'load-library)
        (:prefix ("g" . "goto")
          :desc "Find Function" "f" #'find-function
          :desc "Find Variable" "v" #'find-variable
          :desc "Find Library" "l" #'find-library))

;;;###package
(use-package! elisp-format
  :defer t
  :commands (elisp-format-region elisp-format-buffer)
  :init (map! (:localleader
                :map emacs-lisp-mode-map
                (:prefix ("=" . "format")
                  :desc "Format Region or Buffer" :nvg "=" #'+elisp/format-region-or-buffer
                  :desc "Format Region" :nvg "r" #'elisp-format-region
                  :desc "Format Buffer" :nvg "b" #'elisp-format-buffer))))

;;;###package
(use-package! eval-sexp-fu
  :defer t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)))
