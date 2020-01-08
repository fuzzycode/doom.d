
;; https://emacs.stackexchange.com/questions/62/hide-compilation-window
(defun bl-edit/bury-compile-buffer-if-successful (buffer string)
  (if (and
       (null (string-match ".*exited abnormally.*" string))
       (bound-and-true-p  bl-edit-close-compile-on-success))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        (run-at-time
         (format "%d sec" bl-edit-compile-auto-close-time) nil 'delete-windows-on
         (get-buffer-create "*compilation*")))))

(defun bl-edit/maybe-notify-compile-finish (buffer string)
  "Show an alert when compilation finished, like XCode does"
  (require 'alert)
  (when bl-edit-notify-compile-finished
    (if (string-match "^finished" string)
        (alert "Compilation finished OK!" :title "Compilation Successful" :category 'compile :id 'compile-ok)
      (alert "Compilation Failed" :title "Compilation Failed" :category 'compile :id 'compile-fail))))

;;;###autoload
(add-hook 'compilation-finish-functions #'bl-edit/bury-compile-buffer-if-successful)

;;;###autoload
(add-hook 'compilation-finish-functions #'bl-edit/maybe-notify-compile-finish)
