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
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
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
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))

  (map! (:localleader
         :mode (c++-mode c-mode python-mode)
         (:prefix ("d" . "debug")
          :desc "DAP REPL" :g "'" #'dap-ui-repl
          :desc "DAP Hydra" :g "." #'dap-hydra
          :desc "Delete All Sessions" :g "A" #'dap-delete-all-sessions
          :desc "Continue" :g "c" #'dap-continue
          :desc "Step Into" :g "i" #'dap-step-in
          :desc "Step Out" :g "o" #'dap-step-out
          :desc "Restart Frame" :g "r" #'dap-restart-frame
          :desc "Step (Next)" :g "s" #'dap-next
          :desc "Inspect Thing At Point" :g "v" #'dap-ui-inspect-thing-at-point
          (:prefix ("b" . "breakpoints")
           :desc "Add Breakpoint" :g "a" #'dap-breakpoint-add
           :desc "Toggle Breakpoint" :g "b" #'dap-breakpoint-toggle
           :desc "Breakpoint Condition" :g "c" #'dap-breakpoint-condition
           :desc "Delete All" :g "D" #'dap-breakpoint-delete-all
           :desc "Delete Breakpoint" :g "d" #'dap-breakpoint-delete
           :desc "Hit Condition" :g "h" #'dap-breakpoint-hit-condition
           :desc "Log Message" :g "l" #'dap-breakpoint-log-message)
          (:prefix ("d" . "debugging")
           :desc "Debug" :g "d" #'dap-debug
           :desc "Edit Template" :g "e" #'dap-debug-edit-template
           :desc "Debug Last" :g "l" #'dap-debug-last
           :desc "Debug Recent" :g "r" #'dap-debug-recent)
          (:prefix ("e" . "Eval")
           :desc "Eval" :g "e" #'dap-eval
           :desc "Eval region" :g "r" #'dap-eval-region
           :desc "Eval Thing at Point" :g "t" #'dap-eval-thing-at-point)
          (:prefix ("I" . "inspect")
           :desc "Inspect" :g "i" #'dap-ui-inspect
           :desc "Inspect Region" :g "r" #'dap-ui-inspect-region
           :desc "Inspect Thing at Point" :g "t" #'dap-ui-inspect-thing-at-point)
          (:prefix ("S" . "switch")
           :desc "Switch Stack Frame" :g "f" #'dap-switch-stack-frame
           :desc "Switch Session" :g "s" #'dap-switch-session
           :desc "Switch Thread" :g "t" #'dap-switch-thread)
          (:prefix ("w" . "debug windows")
           :desc "Breakpoints" :g "b" #'dap-ui-breakpoints
           :desc "Locals" :g "l" #'dap-ui-locals
           :desc "Go to Output Buffer" :g "o" #'dap-go-to-output-buffer
           :desc "Sessions" :g "s" #'dap-ui-sessions)))))
