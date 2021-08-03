;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;
;;; <leader>

(map! :leader
      ";" nil ;; Save for later
      "x" nil
      "w" nil
      "h" nil

      :desc "M-x" "<SPC>" #'execute-extended-command
      :desc "Eval Expression" ":" #'eval-expression)

(map!
 "<A-up>" #'join-line
 "<A-down>" (cmd! (delete-indentation 1)))
