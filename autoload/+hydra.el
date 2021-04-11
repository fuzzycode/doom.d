;; -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+magit/timemachine-hydra/body "autoload/+hydra" nil t)
(defhydra +magit/timemachine-hydra (:hint nil
                                          :color pink
                                          :pre (unless (bound-and-true-p git-timemachine-mode)
                                                 (call-interactively 'git-timemachine))
                                          :post (when (bound-and-true-p git-timemachine-mode)
                                                  (git-timemachine-quit)))
  "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit
"
  ("c" git-timemachine-show-current-revision)
  ("g" git-timemachine-show-nth-revision)
  ("p" git-timemachine-show-previous-revision)
  ("n" git-timemachine-show-next-revision)
  ("N" git-timemachine-show-previous-revision)
  ("Y" git-timemachine-kill-revision)
  ("q" nil :exit t))

;;;###autoload (autoload '+magit/blame-hydra/body "autoload/+hydra" nil t)
(defhydra +magit/blame-hydra (:hint nil
                                    :color magenta
                                    :pre (unless (bound-and-true-p magit-blame-mode)
                                           (call-interactively 'magit-blame-addition)))
  "
[_b_] Blame Further [_q_] Quit
"
  ("b" magit-blame-addition)
  ("q" nil :exit (progn
                   (when (bound-and-true-p magit-blame-mode)
                     (magit-blame-quit))
                   (not (bound-and-true-p magit-blame-mode)))))

;;;###autoload (autoload 'smartparens-hydra/body "autoload/+hydra" nil t)
(defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
