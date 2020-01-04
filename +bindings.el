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
