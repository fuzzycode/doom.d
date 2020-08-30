;;; private/folding/config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! origami
  :defer t
  :commands origami-javadoc-parser
  :hook (doom-first-input . global-origami-mode)
  :bind (:map origami-mode-map
         ("<C-tab>" . #'origami-recursively-toggle-node))
  :init (map! (:leader
               (:prefix "x"
                (:prefix ("f" "Folding")
                 :desc "Toggle All Nodes" :g "A" #'origami-toggle-all-nodes
                 :desc "Close Node" :g "c" #'origami-close-node
                 :desc "Close Node Recursively" :g "C" #'origami-close-node-recursively
                 :desc "Open All Nodes" :g "r" #'origami-open-all-nodes
                 :desc "Close All Nodes" :g "m" #'origami-close-all-nodes
                 :desc "Open Node" :g "o" #'origami-open-node
                 :desc "Open Node Recursively" :g "O" #'origami-open-node-recursively
                 :desc "Next Fold" :g "n" #'origami-next-fold
                 :desc "Previous Fold" :g "p" #'origami-previous-fold
                 :desc "Reset Folding" :g "R" #'origami-reset
                 :desc "Recursive Toggle Node" :g "t" #'origami-recursively-toggle-node
                 :desc "Show Only Node" :g "s" #'origami-show-only-node))))
  :config (add-to-list 'origami-parser-alist '(c++-mode . +origami-c-parser)))

;;;###package
(use-package! lsp-origami
  :after lsp
  :hook (lsp-after-open . lsp-origami-try-enable))
