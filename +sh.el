;; -*- lexical-binding: t; -*-

(use-package! sh-script
  :defer t
  :init (map! (:localleader
                :map shell-mode-map
                (:prefix ("i" . "insert")
                  :desc "Insert Shebang" :nvg "!" #'insert-shebang
                  :desc "Case" :nvg "c" #'sh-case
                  :desc "If" :nvg "i" #'sh-if
                  :desc "Function" :nvg "f" #'sh-function
                  :desc "For" :nvg "o" #'sh-for
                  :desc "Indexed For" :nvg "e" #'sh-indexed-loop
                  :desc "While" :nvg "w" #'sh-while
                  :desc "Repeat" :nvg "r" #'sh-repeat
                  :desc "Select" :nvg "s" #'sh-select
                  :desc "Until" :nvg "u" #'sh-until
                  :desc "While Getopts" :nvg "g" #'sh-while-getopts))))
