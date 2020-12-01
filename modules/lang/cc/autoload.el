;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;; The pluses in c++-mode can be annoying to search for ivy/helm (which reads
;; queries as regexps), so we add these for convenience.
;;;###autoload
(defalias 'cpp-mode 'c++-mode)
;;;###autoload
(defvaralias 'cpp-mode-map 'c++-mode-map)
