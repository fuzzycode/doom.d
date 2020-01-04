;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
        (:prefix ("h" . "help")
          (:prefix ("d" . "describe")
            :desc "Function" :g "f" #'describe-function
            :desc "Face" :g "F" #'describe-face
            :desc "Key" :g "k" #'describe-key
            :desc "Mode" :g "m" #'describe-mode
            :desc "Package" :g "p" #'describe-package
            :desc "Theme" :g "t" #'describe-theme
            :desc "Variable" :g "v" #'describe-variable))))

(map! (:leader
        (:prefix ("s" . "search")
          (:prefix ("m" . "multiple cursors")
            :desc "Mark All" :g "a" #'mc/mark-all-dwim
            :desc "Mark All Like This" :g "b" #'mc/mark-all-like-this
            :desc "Mark More Like This" :g "m" #'mc/mark-more-like-this-extended
            :desc "Edit Lines" :g "r" #'mc/edit-lines
            (:prefix ("s" . "insert/sort")
              :desc "Inert Letters" :g "l" #'mc/insert-letters
              :desc "Mark SGML pair" :g "m" #'mc/mark-sgml-tag-pair
              :desc "Insert Numbers" :g "n" #'mc/insert-numbers
              :desc "Sort Regions" :g "s" #'mc/sort-regions
              :desc "Reverse Regions" :g "t" #'mc/reverse-regions)))))
