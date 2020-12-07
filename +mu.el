;;; +mu.el -*- lexical-binding: t; -*-


;; Remove the private folders from recent files as that's where xwidgets stores the temp files used to render the mails as html
(after! recentf
  (add-to-list 'recentf-exclude "/private/var/folders/.*"))

;;;###package
(use-package! mu4e
  :defer t
  :init (setq mu4e-root-maildir (expand-file-name "~/.mail")
              mu4e-drafts-folder "/drafts"
              mu4e-sent-folder   "/sent"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive"
              mu4e-update-interval 120
              mu4e-headers-auto-update t
              mu4e-headers-date-format "%Y-%m-%d %H:%M"
              mu4e-change-filenames-when-moving t
              mu4e-sent-messages-behavior 'delete
              mu4e-use-fancy-chars nil
              mu4e-headers-fields `((:human-date . 18)
                                    (:flags . 4)
                                    (:from-or-to . 20)
                                    (:subject))))

;;;###package
(use-package! mu4e-views
  :after mu4e
  :if (featurep 'xwidget-internal) ;; Test if emacs is built with xwidget support
  :init (setq mu4e-views-completion-method #'ivy
              mu4e-views-default-view-method "html"
              mu4e-views-next-previous-message-behaviour #'stick-to-current-window)
  :config (mu4e-views-mu4e-use-view-msg-method "html"))

;;;###package
(use-package! mu4e-icalendar
  :after mu4e
  :config (mu4e-icalendar-setup))

;;;###package
(use-package! mu4e-compose
  :after mu4e
  :init (setq mu4e-compose-dont-reply-to-self t
              mu4e-compose-keep-self-cc nil
              mu4e-compose-complete-addresses t))

;; EXTERNAL PACKAGES

;;;###package
(use-package! mu4e-alert
  :defer t
  :init (add-hook 'doom-first-input-hook  #'mu4e-alert-enable-mode-line-display)
  :config (mu4e-alert-set-default-style 'notifier))

;;;###package
(use-package mu4e-maildirs-extension
    :defer t
    :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load)))
