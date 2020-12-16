;; -*- lexical-binding: t; -*-

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
;;;###package company
(after! company
  (setq company-tooltip-minimum-width 80
        company-tooltip-maximum-width 80
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(after! (company evil)
  (evil-collection-define-key nil 'company-active-map
    (kbd "C-l") #'company-complete-selection
    (kbd "C-n") #'company-select-next-or-abort
    (kbd "C-p") #'company-select-previous-or-abort
    (kbd "C-j") #'company-select-next-or-abort
    (kbd "C-k") #'company-select-previous-or-abort
    (kbd "M-j") #'company-select-next
    (kbd "M-k") #'company-select-previous)

  (evil-collection-define-key nil 'company-search-map
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort))

;; Show the location of the sym-link and not the actual file
(setq find-file-visit-truename nil
      vc-follow-symlinks nil)

(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

(setq x-stretch-cursor t
      ns-pop-up-frames nil
      create-lockfiles nil
      require-final-newline t
      use-dialog-box nil
      use-file-dialog nil)

;; iSeach
(setq isearch-lax-whitespace t
      search-whitespace-regexp ".*?"
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t)

;; Don't confirm closing emacs with running processes
(setq confirm-kill-processes nil)

;; Backups
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Compilation
(setq compilation-auto-jump-to-first-error nil
      compilation-skip-threshold 2)

;; Enable advanced features without asking
(put 'narrow-to-region 'disabled nil)

(blink-cursor-mode -1) ;; We never want that

(electric-indent-mode -1) ;; Not needed, newline and indent will take care of it

;; Bump the process read limit to 3mb, helps lsp-mode perform better
(setq read-process-output-max (* 3 1024 1024))

;; TODO(Björn Larsson): Remove the need for a hard coded path parts
(after! recentf
  (add-to-list 'recentf-exclude #'directory-name-p) ;; Filter out all directories from the list
  (add-to-list 'recentf-exclude (concat ".*?" "\\.emacs\\.d/\\.local" ".*"))  ;; Remove all files in the .local folders
  (add-to-list 'recentf-exclude "\\.vrb$")) ;; Remove latex intermediate files

;; Configure flycheck, with lsp available there is no need for c/c++-* family
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))

(after! git-gutter-fringe
  ;; Fringe configuration
  (fringe-mode '(8 . 4)))  ;; Left is full size and right is half size, makes room for break-point and tilde bitmaps

;; mac specifics
(when IS-MAC
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-right-command-modifier nil
        mac-right-option-modifier nil))
