
(defun +lsp/dim-lsp-sideline (&rest args)
  (mapcar (lambda (face)
           (when (facep face)
             (set-face-foreground face (face-attribute 'font-lock-comment-face :foreground))))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info)))

;;;###autoload
(add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline)

;;;###autoload
(defun +lsp/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;;###autoload
(defun +lsp/ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

;;;###autoload
(defun +lsp/ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))

;;;###autoload
(defun +lsp/ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))

;;;###autoload
(defun +lsp/ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))

;;;###autoload
(defun +lsp/ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))

;;;###autoload
(defun +lsp/ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; References w/ Role::Role
;;;###autoload
(defun +lsp/ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
    (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
;;;###autoload
(defun +lsp/ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

;; References w/ Role::Dynamic bit (macro expansions)
;;;###autoload
(defun +lsp/ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
;;;###autoload
(defun +lsp/ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))
