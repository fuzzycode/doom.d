
(use-package! sh-script
  :defer t
  :init (map! (:localleader
                :map shell-mode-map
                (:prefix ("i" . "insert")
                  :desc "Insert Shebang" :g "!" #'insert-shebang
                  :desc "Case" :g "c" #'sh-case
                  :desc "If" :g "i" #'sh-if
                  :desc "Function" :g "f" #'sh-function
                  :desc "For" :g "o" #'sh-for
                  :desc "Indexed For" :g "e" #'sh-indexed-loop
                  :desc "While" :g "w" #'sh-while
                  :desc "Repeat" :g "r" #'sh-repeat
                  :desc "Select" :g "s" #'sh-select
                  :desc "Until" :g "u" #'sh-until
                  :desc "While Getopts" :g "g" #'sh-while-getopts))))
