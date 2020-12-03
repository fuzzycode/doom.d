;;; private/spelling/config.el -*- lexical-binding: t; -*-


;;;###package
(use-package! ispell
  :defer t
  :init (setq ispell-program-name "aspell"
              ispell-extra-args (+spelling//detect-ispell-args t))
  :config (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))

;;;###package
(use-package! wucuo
  :defer t
  :commands +spelling//setup-text-mode
  :hook ((prog-mode . wucuo-start)
         (yaml-mode . wucuo-start)
         (conf-mode . wucuo-start)
         (text-mode . +spelling//setup-text-mode))
  :init (setq wucuo-personal-font-faces-to-check '(git-commit-summary)
              wucuo-spell-check-buffer-predicate
              (lambda ()
                (not (memq major-mode
                           '(dired-mode
                             log-edit-mode
                             compilation-mode
                             help-mode
                             profiler-report-mode
                             speedbar-mode
                             gud-mode
                             calc-mode
                             Info-mode))))))

;;;###package
(use-package! flyspell
  :defer t
  :commands +spell-init-flyspell-predicate-h
  :hook (flyspell-mode . +spell-init-flyspell-predicate-h)
  :init (setq flyspell-issue-welcome-flag nil
              flyspell-issue-message-flag nil
              flyspell-mark-duplications-flag nil))

;;;###package
(use-package! flyspell-correct
  :after wucuo
  :commands (flyspell-correct-at-point
             flyspell-correct-wrapper)
  :bind (:map flyspell-mode-map
         ("M-i" . #'flyspell-correct-wrapper))
  :init (setq flyspell-popup-correct-delay 0.8)
  (map! (:leader
         (:prefix "S"
          :desc "Correct Next" :g "n" #'flyspell-correct-next
          :desc "Correct Previous" :g "p" #'flyspell-correct-previous
          :desc "Correct At Point" :g "c" #'flyspell-correct-at-point
          :desc "Correct DWIM" :g "s" #'flyspell-correct-wrapper
          :desc "Change Dictionary" :g "d" #'ispell-change-dictionary))))

;;;###package
(use-package! flyspell-correct-ivy
  :after flyspel-correct
  :when (featurep! :completion ivy)
  :commands (flyspell-correct-ivy)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))
