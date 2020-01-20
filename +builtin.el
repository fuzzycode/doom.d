
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

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

;; Show the location of the sym-link and not the actual file
(setq find-file-visit-truename nil)

(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

(setq x-stretch-cursor t
      ns-pop-up-frames nil
      create-lockfiles nil
      require-final-newline t
      use-dialog-box nil
      use-file-dialog nil)

(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB"))
(setq ispell-dictionary "english")

;; Backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Compilation
(setq compilation-auto-jump-to-first-error t
      compilation-skip-threshold 2)
