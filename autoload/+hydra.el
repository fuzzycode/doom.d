;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+magit/timemachine-hydra/body' "autoload/+hydra" nil t)
(defhydra +magit/timemachine-hydra (:hint nil
                                          :color magenta
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
  ("q" nil :color blue))

;;;###autoload (autoload '+magit/blame-hydra/body' "autoload/+hydra" nil t)
(defhydra +magit/blame-hydra (:hint nil
                                    :color magenta
                                    :pre (unless (bound-and-true-p magit-blame-mode)
                                           (call-interactively 'magit-blame-addition)))
  "
[_b_] Blame Further [_q_] Quit
"
  ("b" magit-blame-addition)
  ("q" nil :color blue :exit (progn (when (bound-and-true-p magit-blame-mode)
                                      (magit-blame-quit))
                                    (not (bound-and-true-p magit-blame-mode))))
  )