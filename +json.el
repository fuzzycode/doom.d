;;; +json.el -*- lexical-binding: t; -*-

(map! (:localleader
        :map json-mode-map
        (:prefix ("=" . "format")
          :desc "Format Region Or Buffer" :nvg "=" #'+json/pretty-print-region-or-buffer
          :desc "Format Buffer" :nvg "b" #'json-pretty-print-buffer
          :desc "Format Region" :nvg "r" #'json-pretty-print)))
