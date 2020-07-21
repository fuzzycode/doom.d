;;; ~/Development/GitHub/dotfiles/doom.d/+mu.el -*- lexical-binding: t; -*-

;; Some things need to be set after doom has configured mu4e
(after! mu4e
  (setq mu4e-update-interval 120
        mu4e-headers-auto-update t
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-change-filenames-when-moving t
        mu4e-sent-messages-behavior 'delete
        mu4e-use-fancy-chars nil
        mu4e-headers-fields `((:human-date . 18)
                              (:flags . 4)
                              (:from-or-to . 20)
                              (:subject))))

;; Remove the private folders from recent files as that's where xwidgets stores the temp files used to render the mails as html
(after! recentf
  (add-to-list 'recentf-exclude "/private/var/folders/.*"))

;;;###package
(use-package! mu4e
  :commands +mu4e/mu4e-action-view-with-xwidget
  :init (setq mu4e-root-maildir (expand-file-name "~/.mail")
              mu4e-drafts-folder "/drafts"
              mu4e-sent-folder   "/sent"
              mu4e-trash-folder  "/trash"
              mu4e-refile-folder "/archive")
  :config (when (fboundp 'imagemagick-register-types)
            (imagemagick-register-types))
  (add-to-list 'mu4e-view-actions '("xView with XWidget" . +mu4e/mu4e-action-view-with-xwidget)))

;;;###package
(use-package! mu4e-view
  :after mu4e
  :init (setq mu4e-view-use-gnus t
              mu4e-view-show-images t))

;;;###package
(use-package! mu4e-icalendar
  :after mu4e
  :config (mu4e-icalendar-setup))

;;;###package
(use-package! mu4e-contrib
  :after mu4e
  :init (setq mu4e-html2text-command 'mu4e-shr2text))

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
