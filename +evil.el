;;; +evil.el -*- lexical-binding: t; -*-

;; Occur mode
(after! evil-collection
  (evil-collection-define-key 'normal 'occur-mode-map
    (kbd "C-c C-e") 'occur-edit-mode
    (kbd "C-x C-q") nil)

  (evil-collection-define-key 'normal 'occur-edit-mode-map
    (kbd "C-c C-c") 'occur-cease-edit
    (kbd "C-x C-q") nil))

(after! outline
  (evil-collection-define-key 'normal 'outline-mode-map
    (kbd "C-j") nil
    (kbd "C-k") nil))

;;;###package evil-surround
(after! evil-surround
  (evil-define-key 'visual evil-surround-mode-map "s" #'evil-surround-region)

  (let ((pairs '((?g "$" . "$")
                 (?h "(" . ")")
                 (?j "[" . "]")
                 (?k "{" . "}")
                 (?l "<" . ">")
                 (?t "`" . "`")
                 (?a "'" . "'")
                 (?s "\"" . "\""))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

;;;###package evil-snipe
(after! evil-snipe
  (setq evil-snipe-spillover-scope 'whole-visible))
