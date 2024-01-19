;; Eldoc mode
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav            ; Make M-. and M-, work in elisp like they do in slime
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(setq debugger-stack-frame-as-list t)

;; Load newest file
(setq load-prefer-newer t)

;; Custom name for bookmark when in a defun
(defun emacs-lisp-custom-record-function ()
  (set (make-local-variable 'bookmark-make-record-function)
       (lambda (&optional no-file no-context posn)
         (let (defun record)
           (setq record (bookmark-make-record-default no-file no-context posn))
           (ignore-errors
             (save-excursion
               (end-of-defun)
               (beginning-of-defun)
               (setq defun (read (current-buffer)))))
           (if (eq (car defun) 'defun)
               (setcar record (format "%s" (cadr defun))))
           record))))

(defun emacs-lisp-add-keywords ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\)"
      1 font-lock-warning-face prepend)))
  ;; (font-lock-add-keywords
  ;;  nil
  ;;  '(("\\<\\(add-hook\\|setq\\)\\>"
  ;;     1 font-lock-keyword-face prepend)))
  )

(defun emacs-lisp-custom-hippie-expand ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(yas/hippie-try-expand
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-dabbrev-closest-first
         try-expand-dabbrev-visible
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-ispell)))

(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-custom-record-function)
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-add-keywords)
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-custom-hippie-expand)

;; C-u C-u C-x C-e does eval and replace
(defun eval-last-sexp-replace (oldfun eval-last-sexp-arg-internal)
  (if (and (called-interactively-p 'interactive)
           (eq 16 (car eval-last-sexp-arg-internal)))
      (call-interactively 'eval-and-replace)
    (funcall oldfun eval-last-sexp-arg-internal)))
(advice-add 'eval-last-sexp :around #'eval-last-sexp-replace)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((form (elisp--preceding-sexp))
        (opoint (point)))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (forward-sexp -1))
    (condition-case error
        (progn
          (setq form (eval form))
          (delete-region (point) opoint)
          (prin1 form (current-buffer)))
      (error (goto-char opoint)
             (message "eval-and-replace: %s" error)))))

;; (defun auto-byte-recompile ()
;;   "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
;; file corresponding to the current buffer file, then recompile the file."
;;   (interactive)
;;   (when (and (eq major-mode 'emacs-lisp-mode)
;;              (file-exists-p (byte-compile-dest-file buffer-file-name)))
;;     (byte-compile-file buffer-file-name)))

;; (add-hook 'after-save-hook #'auto-byte-recompile)

(provide 'init-elisp)
