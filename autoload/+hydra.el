;; -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+bl/timemachine-hydra/body "autoload/+hydra" nil t)
(defhydra +bl/timemachine-hydra (:hint nil
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

;;;###autoload (autoload '+bl/blame-hydra/body "autoload/+hydra" nil t)
(defhydra +bl/blame-hydra (:hint nil
                                    :color pink
                                    :pre (unless (bound-and-true-p magit-blame-mode)
                                           (call-interactively 'magit-blame-addition))
                                    :post (when (bound-and-true-p magit-blame-mode)
                                            (magit-blame-quit)))
  "
[_b_] Blame Further [_q_] Quit
"
  ("b" magit-blame-addition)
  ("q" nil :exit t))

;;;###autoload (autoload '+bl/window-hydra "autoload/+hydra" nil t)
(defhydra +bl/window-hydra (:init nil
                            :color pink)
  "
[_h_] Decrease Width [_j_] Decrease Hight [_k_] Increase Hight [_l_] Increase Width [_q_] Quit
"
  ("h" evil-window-decrease-width)
  ("j" evil-window-decrease-height)
  ("k" evil-window-increase-height)
  ("l" evil-window-increase-width)
  ("q" nil :exit t))
