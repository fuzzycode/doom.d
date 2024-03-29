;; -*- lexical-binding: t; -*-
;;;###if (modulep! :ui hydra)

;;;###autoload (autoload '+bl/timemachine-hydra/body "autoload/+hydra" nil t)
(defhydra +bl/timemachine-hydra (:hint nil
                                          :color pink
                                          :pre (unless (bound-and-true-p git-timemachine-mode)
                                                 (call-interactively 'git-timemachine))
                                          :post (when (bound-and-true-p git-timemachine-mode)
                                                  (git-timemachine-quit)))
  "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_y_] copy hash [_Y_] copy full hash [_q_] quit
"
  ("c" git-timemachine-show-current-revision)
  ("g" git-timemachine-show-nth-revision)
  ("p" git-timemachine-show-previous-revision)
  ("n" git-timemachine-show-next-revision)
  ("N" git-timemachine-show-previous-revision)
  ("y" git-timemachine-kill-abbreviated-revision)
  ("Y" git-timemachine-kill-revision)
  ("q" nil :exit t))

;;;###autoload (autoload '+bl/blame-hydra/body "autoload/+hydra" nil t)
(defhydra +bl/blame-hydra (:hint nil
                                    :color pink
                                    :pre (unless (bound-and-true-p magit-blame-mode)
                                           (call-interactively 'magit-blame-addition))
                                    :post (when (bound-and-true-p magit-blame-mode)
                                            (magit-blame-quit)))
  "
[_b_] Blame Further [_y_] copy hash [_q_] Quit
"
  ("b" magit-blame-addition)
  ("y" magit-blame-copy-hash)
  ("q" nil :exit t))
