
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq bidi-display-reordering nil)
(setq bidi-paragraph-direction 'left-to-right)

(setq ibuffer-formats '((mark modified read-only locked " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))
