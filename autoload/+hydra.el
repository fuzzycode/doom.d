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

;;;###autoload (autoload '+bl/smerge-hydra "autoload/+hydra" nil t)
(defhydra +bl/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

;;;###autoload
(add-hook 'magit-diff-visit-file (lambda ()
                                   (when smerge-mode
                                     (+bl/smerge-hydra))))
