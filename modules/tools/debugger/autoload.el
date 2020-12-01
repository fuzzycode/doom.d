;;; private/debugging/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +debugging/init-dap-mode-cpp ()
  "Setup dap for c++ debugging"
  (require 'dap-gdb-lldb))
