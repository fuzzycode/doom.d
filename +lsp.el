(map! (:localleader
        :mode (c++-mode python-mode)
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
            :desc "Sessions" :g "s" #'dap-ui-sessions))))

(map! (:localleader
        :mode (c++-mode python-mode)
        (:prefix ("=" . "format")
          :desc "Format Dwim" :g "=" #'+lsp/lsp-format-region-or-buffer
          :desc "Format Buffer" :g "b" #'lsp-format-buffer
          :desc "Format Region" :g "r" #'lsp-format-region)
        (:prefix ("r" . "refactor")
          :desc "Rename" :g "r" #'lsp-rename)))

(after! lsp-mode
  (define-key lsp-mode-map (kbd "<A-return>") #'lsp-execute-code-action))

(after! lsp-ui
  (add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline))

(setq dap-breakpoints-file (concat doom-local-dir "cache/dap-breakpoints"))
