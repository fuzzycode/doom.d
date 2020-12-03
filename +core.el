;; -*- lexical-binding: t; -*-

(setq which-key-sort-order 'which-key-key-order-alpha)

(global-set-key (kbd "C-c u") #'undo-fu-only-undo)

(add-hook 'help-mode-hook #'rainbow-mode)

(setq projectile-enable-caching nil)

(after! yasnippet
  (when (file-exists-p "~/.snippets")
    (add-to-list 'yas-snippet-dirs "~/.snippets")
    (yas-reload-all)))

(after! flycheck
  (setq flycheck-error-list-format `[("File" 25)
                                     ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                     ("Col" 3 nil :right-align t)
                                     ("Level" 8 flycheck-error-list-entry-level-<)
                                     ("ID" 35 t)
                                     (#("Message (Checker)" 9 16
                                        (face flycheck-error-list-checker-name))
                                      0 t)]))

(setq ibuffer-formats '((mark modified read-only locked " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))
