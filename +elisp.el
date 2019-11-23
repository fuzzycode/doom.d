
(map! (:localleader
        :map emacs-lisp-mode-map
        (:prefix ("=" . "format")
          :desc "Indent Region or Buffer" :g "=" '+elisp/indent-region-or-buffer
          :desc "Indent Region" :g "r" 'indent-region
          :desc "Indent Buffer" :g "b" '+elisp/indent-buffer)))

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
