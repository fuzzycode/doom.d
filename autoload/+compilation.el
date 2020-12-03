;;; doom.d/autoload/+compilation.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +core/compile-auto-close-time 2
  "Time in seconds to leave a successful compile buffer open.")

;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
;;;###autoload
(defun +core/bury-compile-buffer-if-successful (_buffer string)
  (if (null (string-match ".*exited abnormally.*" string))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        (run-at-time
         (format "%d sec" +core/compile-auto-close-time) nil 'delete-windows-on
         (get-buffer-create "*compilation*")))))

;;;###autoload
(add-hook 'compilation-finish-functions #'+core/bury-compile-buffer-if-successful)

;;;###autoload
(defun +core/maybe-notify-compile-finish (_buffer string)
  "Show an alert when compilation finished, like XCode does"
  (require 'alert)
  (when (not (memq major-mode '(rg-mode))) ;; No need to alert after each search
    (if (string-match "^finished" string)
        (alert "Compilation finished OK!" :title "Compilation Successful" :category 'compile :id 'compile-ok)
      (alert "Compilation Failed" :title "Compilation Failed" :category 'compile :id 'compile-fail))))

;;;###autoload
(add-hook 'compilation-finish-functions #'+core/maybe-notify-compile-finish)
