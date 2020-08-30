;;; private/folding/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +origami-c-parser (create)
  "Include javadoc in c style folding"
  (let ((c-style (origami-c-style-parser create))
        (macros (origami-c-macro-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge
        (origami-fold-shallow-merge
         (origami-fold-root-node (funcall c-style content))
         (origami-fold-root-node (funcall javadoc content)))
        (origami-fold-root-node (funcall macros content)))))))
