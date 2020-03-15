;;; ~/.doom.d/csv.el -*- lexical-binding: t; -*-

(use-package! csv-mode
  :defer t
  :mode "\\.csv$"
  :init (map! :localleader :map csv-mode-map
              :desc "Align Field" :g "a" #'csv-align-field
              :desc "Kill Fields" :g "k" #'csv-kill-fields
              :desc "Header Line" :g "h" #'csv-header-line
              :desc "Toggle Invisibility" :g "" #'csv-toggle-invisibility
              :desc "Forward Field" :g "i" #'csv-forward-field
              :desc "Backward Field" :g "n" #'csv-backward-field
              :desc "Reverse region" :g "p" #'csv-reverse-region
              (:prefix ("s" . "sort")
                :desc "Sort Fields" :g "f" #'csv-sort-fields
                :desc "Sort Numeric Fields" :g "n" #'csv-sort-numeric-fields
                :desc "Toggle Descending" :g "o" #'csv-toggle-descending)
              :desc "Transpose" :g "t" #'csv-transpose
              :desc "Unalign Fields" :g "u" #'csv-unalign-fields
              (:prefix ("y" . "yank")
                :desc "Yank Fields" :g "f" #'csv-yank-fields
                :desc "Yank As New Table" :g "t" #'csv-yank-as-new-table)))
