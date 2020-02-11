;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map! (:leader
        (:prefix ("h" . "help")
          :desc "help map" :g "h" help-map
          :desc "Info" :g "i" #'info
          (:prefix ("d" . "describe")
            :desc "Function" :g "f" #'describe-function
            :desc "Face" :g "F" #'describe-face
            :desc "Key" :g "k" #'describe-key
            :desc "Mode" :g "m" #'describe-mode
            :desc "Package" :g "p" #'describe-package
            :desc "Theme" :g "t" #'describe-theme
            :desc "Variable" :g "v" #'describe-variable
            :desc "Text Properties" :g "T" #'describe-text-properties))))

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

(map! (:leader (:prefix ("x" . "text")
                 :desc "Zoom Text" :g "z" #'+hydra/text-zoom/body)))

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Remove binding, I did not need it and it was colliding with org mode keys
(after! pyenv-mode
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))
