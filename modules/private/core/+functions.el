;;; -*- lexical-binding: t; -*-

;; This whole file is a copy from the awesome Spacemacs package
;; All functions renamed to reduce confusion


(defvar +core/core--toggles '()
  "Store all the toggles defined")


(defun +core/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.
A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.
If there are multiple properties with the same keyword, only the first property
and its values is returned.
Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))


(defmacro +core/add-toggle (name &rest props)
  ""
  (let* ((toggle-func (intern (format "+core/toggle-%s" (symbol-name name))))
         (status-func (intern (format "%s-p" toggle-func)))
         (on-func (intern (format "%s-on" toggle-func)))
         (off-func (intern (format "%s-off" toggle-func)))
         (mode (plist-get props :mode))

         (status (or mode (plist-get props :status)))
         (on-body (if mode `((,mode t))  (+core/mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (+core/mplist-get props :off)))
         (on-message (plist-get props :on-message))
         (off-message (plist-get props :off-message))

         (bind (plist-get props :bind))

         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))

    `(progn
       (push (append '(,name) '(:toggle ,toggle-func :predicate ,status-func) ',props) +core/core--toggles)

       ;; Define the toggle function
       (defun ,toggle-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         (if (,status-func)
             (progn ,@off-body
                    (when (called-interactively-p 'any)
                      (message ,(or off-message (format "%s disabled" (symbol-name name))))))
           (progn ,@on-body
                  (when (called-interactively-p 'any)
                    (message ,(or on-message (format "%s enabled." (symbol-name name))))))))

       ;; Define the status predicate
       (defun ,status-func ()
         ,(format "Check if %s is active." (symbol-name name))
         ,status-eval)

       ,@(when status
           `(
             ;; On function
             (defun ,on-func ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,status-func) (,toggle-func)))

             ;; Off function
             (defun ,off-func ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,status-func) (,toggle-func)))))
       ;; If provided, bind the toggle function to the provided key under the t prefix
       (when ,bind
         (let ((desc (or (plist-get ,bind :desc) name))
               (key  (plist-get ,bind :key)))
           (map! (:leader (:prefix "t" :desc desc :g key ',toggle-func)))))
       )))
