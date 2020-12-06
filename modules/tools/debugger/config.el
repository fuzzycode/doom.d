;;; private/debugging/config.el -*- lexical-binding: t; -*-

;;;###package
(use-package! dap-mode
  :when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
  :commands (dap-breakpoint-toggle dap-debug dap-debug-last)
  :preface (setq dap-breakpoints-file (concat doom-etc-dir "dap-breakpoints")
                 dap-utils-extension-path (concat doom-etc-dir "dap-extension/"))

  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil nil (make-sparse-keymap)
    (when (bound-and-true-p evil-mode)
      (evil-normalize-keymaps))  ; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook so that this minor
    ;; mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))
  :hook ((dap-mode . dap-ui-mode)
         (c-mode-local-vars . +debugging/init-dap-mode-cpp)
         (c++-mode-local-vars . +debugging/init-dap-mode-cpp)
         (dap-session-created . #'+dap-running-session-mode)
         (dap-stopped . #'+dap-running-session-mode)
         (dap-stack-frame-changed . (lambda (session)
                                      (when (dap--session-running session)
                                        (+dap-running-session-mode 1)))))
  :bind (:map lsp-mode-map
         ("<f9>" . dap-breakpoint-toggle)
         ("<f5>" . dap-debug)
         ("S-<f5>" . dap-debug-last)
         :map +dap-running-session-mode-map
         ("<f5>" . dap-continue)
         ("<f10>" . dap-next)
         ("<f11>" . dap-step-in)
         ("S-<f11>" . dap-step-out))
  :init
  (setq dap-auto-configure-features '(sessions locals tooltip))

  (when (executable-find "python3")
    (setq dap-python-executable "python3"))

  (map! (:localleader
         :mode (c++-mode c-mode python-mode)
         (:prefix ("d" . "debug")
          :desc "DAP REPL" :ng "'" #'dap-ui-repl
          :desc "DAP Hydra" :ng "." #'dap-hydra
          :desc "Delete All Sessions" :ng "A" #'dap-delete-all-sessions
          :desc "Continue" :ng "c" #'dap-continue
          :desc "Step Into" :ng "i" #'dap-step-in
          :desc "Step Out" :ng "o" #'dap-step-out
          :desc "Restart Frame" :ng "r" #'dap-restart-frame
          :desc "Step (Next)" :ng "s" #'dap-next
          :desc "Inspect Thing At Point" :ng "v" #'dap-ui-inspect-thing-at-point
          (:prefix ("b" . "breakpoints")
           :desc "Add Breakpoint" :ng "a" #'dap-breakpoint-add
           :desc "Toggle Breakpoint" :ng "b" #'dap-breakpoint-toggle
           :desc "Breakpoint Condition" :ng "c" #'dap-breakpoint-condition
           :desc "Delete All" :ng "D" #'dap-breakpoint-delete-all
           :desc "Delete Breakpoint" :ng "d" #'dap-breakpoint-delete
           :desc "Hit Condition" :ng "h" #'dap-breakpoint-hit-condition
           :desc "Log Message" :ng "l" #'dap-breakpoint-log-message)
          (:prefix ("d" . "debugging")
           :desc "Debug" :ng "d" #'dap-debug
           :desc "Edit Template" :ng "e" #'dap-debug-edit-template
           :desc "Debug Last" :ng "l" #'dap-debug-last
           :desc "Debug Recent" :ng "r" #'dap-debug-recent)
          (:prefix ("e" . "Eval")
           :desc "Eval" :ng "e" #'dap-eval
           :desc "Eval region" :ng "r" #'dap-eval-region
           :desc "Eval Thing at Point" :ng "t" #'dap-eval-thing-at-point)
          (:prefix ("I" . "inspect")
           :desc "Inspect" :ng "i" #'dap-ui-inspect
           :desc "Inspect Region" :ng "r" #'dap-ui-inspect-region
           :desc "Inspect Thing at Point" :ng "t" #'dap-ui-inspect-thing-at-point)
          (:prefix ("S" . "switch")
           :desc "Switch Stack Frame" :ng "f" #'dap-switch-stack-frame
           :desc "Switch Session" :ng "s" #'dap-switch-session
           :desc "Switch Thread" :ng "t" #'dap-switch-thread)
          (:prefix ("w" . "debug windows")
           :desc "Breakpoints" :ng "b" #'dap-ui-breakpoints
           :desc "Locals" :ng "l" #'dap-ui-locals
           :desc "Go to Output Buffer" :ng "o" #'dap-go-to-output-buffer
           :desc "Sessions" :ng "s" #'dap-ui-sessions)))))
