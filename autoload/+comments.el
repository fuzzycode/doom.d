;;; ../Development/GitHub/dotfiles/doom.d/autoload/+comments.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +bl/same-line-p (p1 p2)
  "Check if P1 and P2 are on the same line or not"
  (eq (count-lines 1 p1) (count-lines 1 p2)))

;;;###autoload
(defun +bl/one-line-region-p ()
  "Check if the region is confined to a single line or not"
  (and (region-active-p)
       (+bl/same-line-p (region-beginning) (region-end))))

;;;###autoload
(defadvice! +bl/comment-one-line-blocks-cpp-mode (fn &rest args)
  "In C++ and C if a region is only on one line, it is better to use /* and */ to delimit a comment than to
us // and break the line. This is especially useful when commenting out parameters to functions in c++."
  :around #'evilnc-comment-operator
  (if (and (derived-mode-p 'c-mode 'c++-mode) (+bl/one-line-region-p))
      (let ((comment-start "/*")
            (comment-end "*/")
            (comment-padding ""))
        (apply fn args))
      (apply fn args)))
