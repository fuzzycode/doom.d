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

(map! (:leader (:prefix ("x" . "text")
                 (:prefix ("f" . "folding")
                   :when (featurep! :editor fold)
                   :desc "Close All" :g "C" #'+fold/close-all
                   :desc "Open All" :g "O" #'+fold/open-all
                   :desc "Close" :g "c" #'+fold/close
                   :desc "Open" :g "o" #'+fold/open
                   :desc "Toggle" :g "t" #'+fold/toggle
                   :desc "Next" :g "n" #'+fold/next
                   :desc "Previous" :g "p" #'+fold/previous))))

(map! (:leader (:prefix ("n" . "narrow")
                 :desc "Narrow To Region" :g "r" #'narrow-to-region
                 :desc "Narrow To Defun" :g "d" #'narrow-to-defun
                 :desc "Narrow To Page" :g "p" #'narrow-to-page
                 :desc "Widen" :g "w" #'widen)))

(map! (:leader (:prefix ("k" . "pairs")
        :desc "Barf Forward" :g "b" #'sp-forward-barf-sexp
        :desc "Barf Backward" :g "B" #'sp-backward-barf-sexp
        :desc "Slurp Forward" :g "s" #'sp-forward-slurp-sexp
        :desc "Slurp Backward" :g "S" #'sp-backward-slurp-sexp
        :desc "Absorb Sexp" :g "a" #'sp-absorb-sexp
        :desc "Convolute Sexp" :g "c" #'sp-convolute-sexp
        :desc "Previous Sexp" :g "p" #'sp-previous-sexp
        :desc "Next Sexp" :g "n" #'sp-next-sexp
        :desc "Transpose Sexp" :g "t" #'sp-transpose-sexp
        :desc "Split Sexp" :g "s" #'sp-split-sexp
        :desc "Splice Sexp" :g "S" #'sp-splice-sexp)))

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Remove binding, I did not need it and it was colliding with org mode keys
(after! pyenv-mode
  (define-key pyenv-mode-map (kbd "C-c C-s") nil))
