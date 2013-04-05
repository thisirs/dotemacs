(require 'paredit)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; From https://github.com/purcell/emacs.d.git
;; Use paredit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

;; All-the-way extra command
(require 'paredit-ext)

(define-key paredit-mode-map (kbd "C-)")
  (lambda (arg)
    (interactive "P")
    (if arg
        (paredit-slurp-all-the-way-forward)
      (paredit-forward-slurp-sexp))))

(define-key paredit-mode-map (kbd "C-(")
  (lambda (arg)
    (interactive "P")
    (if arg
        (paredit-slurp-all-the-way-backward)
      (paredit-backward-slurp-sexp))))

(define-key paredit-mode-map (kbd "C-c C-)")
  (lambda (arg)
    (interactive "P")
    (if arg
        (paredit-barf-all-the-way-forward)
      (paredit-forward-barf-sexp))))

(define-key paredit-mode-map (kbd "C-c C-(")
  (lambda (arg)
    (interactive "P")
    (if arg
        (paredit-barf-all-the-way-backward)
      (paredit-backward-barf-sexp))))

;; Same as raise but do not kill
(defun paredit-extract-sexp (&optional argument)
  (interactive "P")
  (cond ((paredit-in-string-p)
         (goto-char (car (paredit-string-start+end-points))))
        ((paredit-in-char-p)
         (backward-sexp))
        ((paredit-in-comment-p)
         (error "No S-expression to raise in comment.")))
  (let* ((n (prefix-numeric-value argument))
         (end (scan-sexps (point) 1))
         (sexp (delete-and-extract-region
                (paredit-point-at-sexp-start)
                end)))
    (backward-up-list n)
    (save-excursion
      (insert sexp "\n"))
    (save-excursion
      (indent-region (point) (+ 1 end)))))

(define-key paredit-mode-map (kbd "M-R") 'paredit-extract-sexp)

(provide 'init-paredit)
