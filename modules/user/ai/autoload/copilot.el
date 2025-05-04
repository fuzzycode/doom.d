;;; tools/ai/autoload/copilot.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bl/file-hidden-p (filename)
  "Return t if FILENAME is hidden.
On Unix-like systems, this means the file starts with a dot.
On Windows, this checks the file attributes."
  (let ((basename (file-name-nondirectory filename)))
    (or (string-prefix-p "." basename) ;; Unix-style hidden files
        (and (eq system-type 'windows-nt) ;; Windows hidden attribute
             (file-exists-p filename)
             (not (string-match-p "/\\.\\./?$" filename)) ;; Avoid "." and ".."
             (let ((attrs (file-attributes filename 'string)))
               (and attrs (string-match-p "H" (nth 8 attrs)))))))) ;; Check Windows hidden flag

;;;###autoload
(defun +bl/point-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (let ((state (syntax-ppss)))
    (or (nth 3 state) (nth 4 state))))

;;;###autoload
(defun +bl/enable-copilot-p ()
  "Check if copilot should be disabled or not"
  (and (not (+bl/file-hidden-p (or buffer-file-name "")))  ;; Disable for hidden files
       (+bl/point-in-string-or-comment-p))) ;;Only allow copilot in strings and comments
